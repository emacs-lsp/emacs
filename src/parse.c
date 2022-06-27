#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "stdio.h"
#include "subprocess.h"

int
main ()
{
  struct subprocess_s *process
    = malloc (sizeof (struct subprocess_s));

  const char *const commandLine[] = { "/bin/cat", "/tmp/msg.txt", 0 };

  subprocess_create (commandLine, subprocess_option_enable_async,
                     process);

  unsigned index = 0;
  char* data[1000] = { 0 };
  for (;;)
    {
      unsigned bytes_read = 0;
      bytes_read = subprocess_read_stdout (process, data + index,
                                           sizeof (data) - 1 - index);
      index += bytes_read;
      if (bytes_read == 0)
        {
          break;
        }
      char *content_length_start;
      char *content_length_end;
      char *cursor = data;

      while (
        ((content_length_start = strstr (cursor, "Content-Length:"))
         != NULL)
        && ((content_length_end
             = strstr (content_length_start, "\r\n\r\n"))
            != NULL))
        {
          char *_end;
          const long content_length
            = strtol (content_length_start
                        + strlen ("Content-Length:"),
                      &_end, 10);
          char *msg = malloc (content_length + 1);
          size_t remaining = strlen (content_length_end + 4);
          strncpy (msg, content_length_end + 4,
                   content_length < remaining ? content_length
                                              : remaining);
          int has_to_read = content_length - remaining;
          while (has_to_read > 0)
            {
              bytes_read
                = subprocess_read_stdout (process,
                                          msg
                                            + (content_length
                                               - has_to_read),
                                          has_to_read);
              has_to_read -= bytes_read;
            }

          msg[content_length] = '\0';
          printf ("message = %s\n", msg);

          cursor = content_length_end + 4 + content_length;
        }

      index = strlen (cursor);
      strncpy (data, cursor, index);
      data[index] = '\0';
      memset (data + index, 0, sizeof (data) - index - 1);
    }
  return 1;
}
