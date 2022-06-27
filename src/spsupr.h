#pragma once

#include <stdlib.h>

struct SSP_Handle {
  /* Returns # of bytes sent or -1 on error */
  int (*send) (struct SSP_Handle *ssph, void *buf, size_t sz);
  /* Returns 0 if no data, 1 if data received and updates ..._buf_sz,
   * -1 on error */
  int (*recv) (struct SSP_Handle *ssph, void *stdout_buf, size_t *stdout_buf_sz,
	       void *stderr_buf, size_t *stderr_buf_sz);
  /* Interrupts pending recv() so that it will immediately exit with 0 */
  void (*cancel_recv) (struct SSP_Handle *ssph);
  /* Returns non-zero while sub-process exists */
  int (*isalive) (struct SSP_Handle *ssph);
  void (*close) (struct SSP_Handle *ssph);
  int pid;
};

struct SSP_Opts {
	const char *binary;
	char *const *argv; /* NULL-terminated */
	char *const *envp; /* NULL-terminated */
	/* 0 for polling, -1 for wait forever */
	int read_timeout_ms;
};

struct SSP_Handle *ssp_spawn(struct SSP_Opts *opts);
