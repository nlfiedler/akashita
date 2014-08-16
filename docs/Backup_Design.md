# Backup Design

## Notes from Prototype

* Running tar through xz and split takes 2 minutes for each 128mb archive file;
    * This seems like a really long time;
    * It makes no sense to compress images;
    * Process is otherwise doing nothing useful for several hours (16gb => 144 files).

## Producer/Consumer Design

* Supervisor ensures Coordinator is running
* Coordinator decides if it is time to begin the backup process
    * Must remember the name of the vault that is being used for the current session
    * Based on configuration, either go immediately or set an alarm
    * If so, set up the worker pipeline described below
        * Load any previously stored state from mnesia, pass to workers init
    * Pipeline is supervised by a supervisor process
    * If time is up, send messages to the workers to stop
    * Workers send message to coordinator with their state
        * Coordinator saves this state in mnesia
* Scanner traverses a directory structure, emitting file paths.
    * Should it save state in mnesia so it can pick up where it left off in the event of failure?
    * How to make it wait for consumer to take files?
        * Need some pattern for producer/consumer with message-passing
    * How can consumer indicate when to save state?
* Builder collects files into a tar using erl_tar module;
    * Based on configuration, creates a compressed tar file;
    * Keeps track of total size of files processed so far;
    * If reaches the archive size limit (e.g. 128mb),
        * Close the tar file
        * Emit the tar file path to consumer
        * Start over with a new tar file
    * If this process fails, restart Scanner from last good state?
* Uploader receives tar files and uploads to the vault

## Configuration

* Option to specify compression setting
    * For images, makes no sense to try to compress further

## Questions

* What do the supervisors supervise?
* How are errors handled (by the supervisors)?
* How do the workers restart after failure?
