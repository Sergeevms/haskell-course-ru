#include <errno.h>
#include <unistd.h>
#include "readlines.h"


size_t rl_max_size (RL *rl)
{
  if (rl != NULL)
    return rl->max_size;
  else
    return 0;
}

RL * rl_open(int fd, size_t max_size)
{
  RL * rl = malloc(sizeof(RL));
  if (rl == NULL)
    {
      return NULL;
    }
  rl->fd = fd;
  rl->max_size = max_size;
  rl->buffer = malloc((max_size + 1) * sizeof(char));
  if (rl->buffer == NULL)
    {
      free(rl);
      return NULL;
    }
  rl->readed = 0;
  rl->skip_next = 0;
}

int rl_close(RL *rl)
{
  if (rl == NULL)
    return 0;
  else
    {
      int rw = close(rl->fd);
      free(rl->buffer);
      free(rl);
      return rw;
    }
}

int rl_readline(RL* rl, char * buf, size_t buffer_size)
{
  if (rl == NULL || buf == NULL)
    {
      errno = EFAULT;
      return -1;
    }
  size_t readed = rl->readed;
  if (readed == 0)
    readed = read(rl->fd, rl->buffer, rl->max_size + 1);
  if (readed == 0)
    return NULL;
  size_t cur_s_length = 0;
  for (; 1 ;)
    {
      for (; rl->buffer[cur_s_length] != '\n' && cur_s_length < readed; ++cur_s_length);
      if (rl->buffer[cur_s_length] != '\n')
	{
	  if (cur_s_length == rl->max_size + 1)
	    {
	      rl->skip_next = 1;
	      rl->readed = 0;
	      return -3;
	    }
	  else
	    {
	      readed += read(rl->fd, rl->buffer[readed], rl->max_size + 1 - readed);
	    }
	}
      else
	{
	  ++cur_s_length;
	  if (rl->skip_next)
	    {
	      rl->skip_next = 0;
	      memmove(rl->buffer, rl->buffer + cur_s_length, sizeof(char) * readed - cur_s_length);
	      return -3;
	    }
	  else
	    {
	      if (buffer_size < cur_s_length)
		{
		  memmove(rl->buffer, rl->buffer + cur_s_length, sizeof(char) * readed - cur_s_length);
		  rl->readed = readed - cur_s_length;
		  return -2;
		}
	      else
		{
		  memcpy(buf, rl->buffer, sizeof(char) * cur_s_length);
		  memmove(rl->buffer, rl->buffer + cur_s_length, sizeof(char) * readed - cur_s_length);
		  rl->readed = readed - cur_s_length;
		  return cur_s_length;
		}
	    }
	}
    }
}
