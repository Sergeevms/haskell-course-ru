#include <unistd.h>
#include <errno.h>
#include "readlines.h"

int main(int argc, char ** argv)
{
  if (argc != 2)
    {
      return -1;
    }
  else
    {
      size_t max_size = atoi(argv[1]);
      RL * reader = rl_open(0, max_size);
      char * buffer = malloc(sizeof(char) * (max_size + 1));
      int status = 1;
      if (errno)
	{
	  perror();
	  return -1;
	}
      while (status != 0)
	{
	  status = rl_readline(reader, buffer, max_size + 1);
	  if (status > 0)
	    {
	      size_t printed = 0;
	      while (printed != status)
		{
		  printed += write(1, buffer, (status - printed) * sizeof(char));
		}
	    }
	}
    }
  return 0;
}
