/**
 * @file execute.c
 *
 * @brief Implements interface functions between Quash and the environment and
 * functions that interpret an execute commands.
 *
 * @note As you add things to this file you may want to change the method signature
 */

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "deque.h"
#include "execute.h"
#include "quash.h"

#define BUFFER_SIZE PATH_MAX + 1
#define READ_END 0
#define WRITE_END 1

// Generate the pid deque structure and implement the methods
IMPLEMENT_DEQUE_STRUCT(pid_queue, int);
PROTOTYPE_DEQUE(pid_queue, int);
IMPLEMENT_DEQUE(pid_queue, int);

/**
 * @brief A job is defined as a single command or a list of commands
 * separated by pipes
 */
typedef struct Job {
  int jobId;   // a unique job id
  pid_queue pids; // the pid of a child process tied to the job
  char* cmd;   // command used to invoke the job
} Job;

// Generate the Jobs deque structure and implement the methods
IMPLEMENT_DEQUE_STRUCT(job_queue, Job);
PROTOTYPE_DEQUE(job_queue, Job);
IMPLEMENT_DEQUE(job_queue, Job);

// Declaration for the pid and job queues
pid_queue processQueue;
job_queue jobQueue;
bool initQueues = true;

// keep track of how many jobs are running
int jobsCount = 0;

/* Create a global tracker for active pipes */
static int pipes[2][2];
static int prev_pipe = -1;
static int next_pipe = 0;


/***************************************************************************
 * Interface Functions
 ***************************************************************************/
// Return a string containing the current working directory.
char* get_current_directory(bool* should_free) {
  // set buffer size of PATH_MAX - maximum number of bytes in a pathname
  char* cwd_path;
  char buf[BUFFER_SIZE];
  cwd_path = getcwd(buf, BUFFER_SIZE);

  *should_free = false;

  return cwd_path;
}

// Returns the value of an environment variable env_var
const char* lookup_env(const char* env_var) {
  char* env_ptr = getenv(env_var);
  if (env_ptr == NULL) {
    perror("ERROR: Could not find environment variable.");
  }
  return env_ptr;
}

// Check the status of background jobs
void check_jobs_bg_status() {
  // Check on the statuses of all processes belonging to all background
  // jobs. This function should remove jobs from the jobs queue once all
  // processes belonging to a job have completed.
  size_t jobQueueLength = length_job_queue(&jobQueue);
  for (int i = 0; i < jobQueueLength; i++) {
    Job job = pop_front_job_queue(&jobQueue);
    size_t pidLength = length_pid_queue(&job.pids);

    int processStatus;
    bool processesDone = false;

    for (int j = 0; j < pidLength; j++) {
      int process = pop_front_pid_queue(&job.pids);
      if (waitpid(process, &processStatus, 0) == process) {
        processesDone = true;
      }
      push_back_pid_queue(&job.pids, process);
    }

    if (!processesDone) {
      push_back_job_queue(&jobQueue, job);
    } else {
      print_job_bg_complete(job.jobId, peek_front_pid_queue(&job.pids), job.cmd);
    }
  }
}

// Prints the job id number, the process id of the first process belonging to
// the Job, and the command string associated with this job
void print_job(int job_id, pid_t pid, const char* cmd) {
  printf("[%d]\t%8d\t%s\n", job_id, pid, cmd);
  fflush(stdout);
}

// Prints a start up message for background processes
void print_job_bg_start(int job_id, pid_t pid, const char* cmd) {
  printf("Background job started: ");
  print_job(job_id, pid, cmd);
}

// Prints a completion message followed by the print job
void print_job_bg_complete(int job_id, pid_t pid, const char* cmd) {
  printf("Completed: \t");
  print_job(job_id, pid, cmd);
}

/***************************************************************************
 * Functions to process commands
 ***************************************************************************/
// Run a program reachable by the path environment variable, relative path, or
// absolute path
void run_generic(GenericCommand cmd) {
  // Execute a program with a list of arguments. The `args` array is a NULL
  // terminated (last string is always NULL) list of strings. The first element
  // in the array is the executable
  char* exec = cmd.args[0];
  char** args = cmd.args;

  execvp(exec, args);

  perror("ERROR: Failed to execute program");
}

// Print strings
void run_echo(EchoCommand cmd) {
  // Print an array of strings. The args array is a NULL terminated (last
  // string is always NULL) list of strings.
  char** str = cmd.args;

  for (int i = 0; str[i] != '\0'; i++) {
    printf("%s ", str[i]);
  }

  printf("\n");

  // Flush the buffer before returning
  fflush(stdout);
}

// Sets an environment variable
void run_export(ExportCommand cmd) {
  // Write an environment variable
  const char* env_var = cmd.env_var;
  const char* val = cmd.val;

  // If name does exist in the environment, then its value is changed to value
  // if overwrite is nonzero; if overwrite is zero, then the value of name
  // is not changed.
  if (setenv(env_var, val, 1) == -1) {
    perror("ERROR: Unable to set environment variable");
  }

}

// Changes the current working directory
void run_cd(CDCommand cmd) {
  // Get the directory name
  const char* dir = cmd.dir;

  // Check if the directory is valid
  if (dir == NULL) {
    perror("ERROR: Failed to resolve path");
  }

  char* cwd_path;
  char buf[BUFFER_SIZE];
  cwd_path = getcwd(buf, BUFFER_SIZE);
  setenv("OLD_PWD", cwd_path, 1);

  // Change directory; returns 0 if successful, -1 otherwise
  if (chdir(dir) == -1) {
    perror("ERROR: Failed to change working directory");
    return;
  }

  cwd_path = getcwd(buf, BUFFER_SIZE);
  setenv("PWD", cwd_path, 1);

}

// Sends a signal to all processes contained in a job
void run_kill(KillCommand cmd) {
  int signal = cmd.sig;
  int job_id = cmd.job;

  // Kill all processes associated with a background job
  size_t jobQueueLength = length_job_queue(&jobQueue);
  for (int i = 0; i < jobQueueLength; i++) {
    Job job = pop_front_job_queue(&jobQueue);
    if (job_id == job.jobId) {
      size_t pidQueueLength = length_pid_queue(&job.pids);
      for (int j = 0; j < pidQueueLength; j++) {
        int pid = pop_front_pid_queue(&job.pids);
        kill(pid, signal);
        push_back_pid_queue(&job.pids, pid);
      }
    }
    push_back_job_queue(&jobQueue, job);
  }
}


// Prints the current working directory to stdout
void run_pwd() {
  char* cwd_path;
  char buf[BUFFER_SIZE];
  cwd_path = getcwd(buf, BUFFER_SIZE);

  printf("%s\n", cwd_path);

  // Flush the buffer before returning
  fflush(stdout);
}

// Prints all background jobs currently in the job list to stdout
void run_jobs() {
  // Print background jobs
  size_t jobQueueLength = length_job_queue(&jobQueue);
  for (int i = 0; i < jobQueueLength; i++) {
    Job job = pop_front_job_queue(&jobQueue);
    print_job(job.jobId, peek_front_pid_queue(&job.pids), job.cmd);
    push_back_job_queue(&jobQueue, job);
  }

  // Flush the buffer before returning
  fflush(stdout);
}

/***************************************************************************
 * Functions for command resolution and process setup
 ***************************************************************************/

/**
 * @brief A dispatch function to resolve the correct @a Command variant
 * function for child processes.
 *
 * This version of the function is tailored to commands that should be run in
 * the child process of a fork.
 *
 * @param cmd The Command to try to run
 *
 * @sa Command
 */
void child_run_command(Command cmd) {
  CommandType type = get_command_type(cmd);

  switch (type) {
  case GENERIC:
    run_generic(cmd.generic);
    break;

  case ECHO:
    run_echo(cmd.echo);
    break;

  case PWD:
    run_pwd();
    break;

  case JOBS:
    run_jobs();
    break;

  case EXPORT:
  case CD:
  case KILL:
  case EXIT:
  case EOC:
    break;

  default:
    fprintf(stderr, "Unknown command type: %d\n", type);
  }
}

/**
 * @brief A dispatch function to resolve the correct @a Command variant
 * function for the quash process.
 *
 * This version of the function is tailored to commands that should be run in
 * the parent process (quash).
 *
 * @param cmd The Command to try to run
 *
 * @sa Command
 */
void parent_run_command(Command cmd) {
  CommandType type = get_command_type(cmd);

  switch (type) {
  case EXPORT:
    run_export(cmd.export);
    break;

  case CD:
    run_cd(cmd.cd);
    break;

  case KILL:
    run_kill(cmd.kill);
    break;

  case GENERIC:
  case ECHO:
  case PWD:
  case JOBS:
  case EXIT:
  case EOC:
    break;

  default:
    fprintf(stderr, "Unknown command type: %d\n", type);
  }
}

/**
 * @brief Creates one new process centered around the @a Command in the @a
 * CommandHolder setting up redirects and pipes where needed
 *
 * @note Processes are not the same as jobs. A single job can have multiple
 * processes running under it. This function creates a process that is part of a
 * larger job.
 *
 * @note Not all commands should be run in the child process. A few need to
 * change the quash process in some way
 *
 * @param holder The CommandHolder to try to run
 *
 * @sa Command CommandHolder
 */
void create_process(CommandHolder holder) {
  // Read the flags field from the parser
  bool p_in  = holder.flags & PIPE_IN;
  bool p_out = holder.flags & PIPE_OUT;
  bool r_in  = holder.flags & REDIRECT_IN;
  bool r_out = holder.flags & REDIRECT_OUT;
  bool r_app = holder.flags & REDIRECT_APPEND; // This can only be true if r_out is true

  // Setup pipes, redirects, and new process
  pid_t m_pid = fork();

  if (p_out) {
    pipe(pipes[next_pipe]);
  }

  if (m_pid == 0) {
    // child process

    if (p_in) {
      dup2(pipes[prev_pipe][READ_END], STDIN_FILENO);
      close(pipes[prev_pipe][WRITE_END]);
    }

    if (p_out) {
      dup2(pipes[next_pipe][WRITE_END], STDOUT_FILENO);
      close(pipes[next_pipe][READ_END]);
    }

    if (r_in) {
      int file_desc = open(holder.redirect_in, O_RDONLY);
      dup2(file_desc, 0);
      close(file_desc);
    }

    if (r_out) {
      if (r_app) {
        int file_desc = open(holder.redirect_out, O_RDWR | O_APPEND | O_CREAT, 0777);
        dup2(file_desc, 1);
        close(file_desc);
      } else {
        int file_desc = open(holder.redirect_out, O_RDWR | O_TRUNC | O_CREAT, 0777);
        dup2(file_desc, 1);
        close(file_desc);
      }
    }

    child_run_command(holder.cmd);
    exit(EXIT_SUCCESS);

  } else {
    // parent process

    if (p_in) {
      close(pipes[prev_pipe][READ_END]);
    }

    if (p_out) {
      close(pipes[next_pipe][WRITE_END]);
    }


    /* Update the pipe trackers for next iteration */
    next_pipe = (next_pipe + 1) % 2;
    prev_pipe = (prev_pipe + 1) % 2;

    push_back_pid_queue(&processQueue, m_pid);
    parent_run_command(holder.cmd);
  }

}

// Run a list of commands
void run_script(CommandHolder* holders) {
  if (holders == NULL)
    return;

  // instantiate the pid and job queues
  if (initQueues) {
    processQueue = new_pid_queue(1);
    jobQueue = new_job_queue(1);
    initQueues = false;
  }

  check_jobs_bg_status();

  if (get_command_holder_type(holders[0]) == EXIT &&
      get_command_holder_type(holders[1]) == EOC) {
    // destroy_pid_queue(&processQueue);
    // destroy_job_queue(&jobQueue);
    end_main_loop();
    return;
  }

  CommandType type;

  // Run all commands in the `holder` array
  for (int i = 0; (type = get_command_holder_type(holders[i])) != EOC; ++i) {
    create_process(holders[i]);
  }

  if (!(holders[0].flags & BACKGROUND)) {
    // A foreground job
    while(!is_empty_pid_queue(&processQueue)) {
      int status;
      int pid = pop_back_pid_queue(&processQueue);
      waitpid(pid, &status, 0);
    }
  }
  else {
    // A new background job.
    jobsCount++;

    // Push the new job to the job queue
    Job job;
    job.jobId = jobsCount;
    job.pids = processQueue;
    job.cmd = get_command_string();
    push_back_job_queue(&jobQueue, job);
    print_job_bg_start(job.jobId, peek_front_pid_queue(&job.pids), job.cmd);
  }

}
