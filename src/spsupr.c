#include "spsupr.h"

#if defined(__linux__) || defined(__FreeBSD__)
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <assert.h>
#include <err.h>
#include <errno.h>
#include <poll.h>
#include <stddef.h>
#include <string.h>
#include <sysexits.h>
#include <unistd.h>

enum { parent_end = 0, child_end = 1 };

struct SSP_Posix {
	struct SSP_Handle handle;
	struct pollfd fds[3];
	pid_t pid;
	int io_fd; /* two-way: sub-process' stdin and stdout */
	int err_fd; /* to read sub-process' stderr */
	/* Sending a byte at write end will unblock a waiting recv() */
	int cancelwr_fd, cancelrd_fd;
	int timeout_ms; /* 0 for poll, -1 for wait forever */
	int isalive;
};
#define GETH(_h) \
	((struct SSP_Posix *)((char *)_h - offsetof(struct SSP_Posix, handle)))

static int
ssp_send(struct SSP_Handle *ssph, void *buf, size_t sz)
{
	struct SSP_Posix *h = GETH(ssph);
	ssize_t res;
	do
		res = send(h->io_fd, buf, sz, MSG_NOSIGNAL);
	while (res == -1 && errno == EINTR);
	return (int)res;
}

static int
ssp_recv(struct SSP_Handle *ssph, void *stdout_buf, size_t *stdout_buf_sz,
	 void *stderr_buf, size_t *stderr_buf_sz)
{
	struct SSP_Posix *h = GETH(ssph);
	const size_t sz1 = *stdout_buf_sz, sz2 = *stderr_buf_sz;
	int ready, have_data, have_err = 0;
	ssize_t len;

	*stdout_buf_sz = *stderr_buf_sz = 0;
	do
		ready = poll(h->fds, 3, h->timeout_ms);
	while (ready == -1 && errno == EINTR);
	if (!ready)
		return 0;

	if (ready && (h->fds[0].revents & POLLIN) == POLLIN) {
		do
			len = recv(h->io_fd, stdout_buf, sz1, MSG_DONTWAIT);
		while (len == -1 && errno == EINTR);
		if (len > 0) {
			*stdout_buf_sz = (size_t)len;
		} else if (!len) {
			h->isalive = 0;
		} else {
			have_err = 1;
		}
		--ready;
	}
	if (ready && (h->fds[1].revents & POLLIN) == POLLIN) {
		do
			len = recv(h->err_fd, stderr_buf, sz2, MSG_DONTWAIT);
		while (len == -1 && errno == EINTR);
		if (len > 0) {
			*stderr_buf_sz = (size_t)len;
		} else if (!len) {
			h->isalive = 0;
		} else {
			have_err = 1;
		}
		--ready;
	}
	if (ready && (h->fds[2].revents & POLLIN) == POLLIN) {
		char buf[20];
		do
			len = recv(h->cancelrd_fd, buf, sizeof(buf),
				   MSG_DONTWAIT);
		while (len == -1 && errno == EINTR);
		--ready;
	}
	have_data = *stdout_buf_sz || *stderr_buf_sz;
	return have_data ? 1 : have_err ? -1 : 0;
}

static void
ssp_cancel_recv(struct SSP_Handle *ssph)
{
	struct SSP_Posix *h = GETH(ssph);
	send(h->cancelwr_fd, h, 1, 0);
}

static int
ssp_isalive(struct SSP_Handle *ssph)
{
	struct SSP_Posix *h = GETH(ssph);
	return h->isalive;
}

static void
ssp_close(struct SSP_Handle *ssph)
{
	struct SSP_Posix *h = GETH(ssph);
	int status;
	close(h->cancelrd_fd);
	close(h->cancelwr_fd);
	close(h->err_fd);
	close(h->io_fd);
	(void)waitpid(h->pid, &status, WNOHANG);
	free(h);
}

struct SSP_Handle *
ssp_spawn(struct SSP_Opts *opts)
{
	int io_fds[2] = { -1, -1 }; /* two-way: stdin & stdout */
	int err_fds[2] = { -1, -1 }; /* stderr, read-only */
	int unbl_fds[2] = { -1, -1 };
	int eno;
	pid_t pid = -1;
	struct SSP_Posix *res = NULL;

	assert(opts);
	assert(opts->binary);

	res = (struct SSP_Posix *)calloc(1, sizeof(*res));
	if (!res)
		return NULL;

	if (socketpair(AF_LOCAL, SOCK_STREAM, 0, io_fds) == -1 ||
	    socketpair(AF_LOCAL, SOCK_STREAM, 0, err_fds) == -1 ||
	    socketpair(AF_LOCAL, SOCK_STREAM, 0, unbl_fds) == -1)
		goto abort;
	pid = fork();
	if (pid == 0) {
		/* Child: */
		char *const *it;
		close(io_fds[parent_end]);
		close(err_fds[parent_end]);
		close(unbl_fds[1]);
		close(unbl_fds[0]);
		if (dup2(io_fds[child_end], STDIN_FILENO) == -1 ||
		    dup2(io_fds[child_end], STDOUT_FILENO) == -1 ||
		    dup2(err_fds[child_end], STDERR_FILENO) == -1)
			err(EX_OSERR, "dup2");

		if (opts->envp)
			for (it = opts->envp; *it; ++it)
				putenv(*it);

		if (opts->binary[0] == '/' || opts->binary[0] == '.') {
			/* Assume path is included */
			execv(opts->binary, opts->argv);
		} else {
			execvp(opts->binary, opts->argv);
		}
		err(EX_OSERR, "cannot execute '%s'", opts->binary);
		exit(EX_OSERR);

	} else if (pid != -1) {
		close(io_fds[child_end]);
		close(err_fds[child_end]);
		res->handle.send = &ssp_send;
		res->handle.recv = &ssp_recv;
		res->handle.cancel_recv = &ssp_cancel_recv;
		res->handle.isalive = &ssp_isalive;
		res->handle.close = &ssp_close;
		res->handle.pid = pid;
		res->fds[0].fd = io_fds[parent_end];
		res->fds[0].events = POLLIN;
		res->fds[1].fd = err_fds[parent_end];
		res->fds[1].events = POLLIN;
		res->fds[2].fd = unbl_fds[parent_end];
		res->fds[2].events = POLLIN;
		res->pid = pid;
		res->io_fd = io_fds[parent_end];
		res->err_fd = err_fds[parent_end];
		res->cancelwr_fd = unbl_fds[child_end];
		res->cancelrd_fd = unbl_fds[parent_end];
		res->timeout_ms = opts->read_timeout_ms;
		res->isalive = 1;
		return &res->handle;

	} else {
		goto abort;
	}
abort:
	eno = errno;
	if (unbl_fds[1] != -1)
		close(unbl_fds[1]);
	if (unbl_fds[0] != -1)
		close(unbl_fds[0]);
	if (err_fds[1] != -1)
		close(err_fds[1]);
	if (err_fds[0] != -1)
		close(err_fds[0]);
	if (io_fds[1] != -1)
		close(io_fds[1]);
	if (io_fds[0] != -1)
		close(io_fds[0]);
	if (res)
		free(res);
	errno = eno;
	return NULL;
}

#else
#error Platform is not supported
#endif /* POSIX */
