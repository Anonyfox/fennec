/* Compilation unit for the vendored LMDB (mdb.c stays byte-for-byte upstream).

   Burrow begins a write txn on the Eio domain thread and commits it from a systhread
   (Eio_unix.run_in_systhread around ml_txn_commit_blocking), so LMDB's writer lock must not be
   owned by a thread. LMDB's Linux default is a robust pthread mutex, which IS thread-owned: the
   cross-thread unlock fails with EPERM, the mutex stays held, and the next write txn on the domain
   thread deadlocks (every durable burrow test hung on ubuntu CI). macOS already uses POSIX
   semaphores, which are not owned. On Linux select SysV semaphores instead: not thread-owned, and
   still crash-robust (SEM_UNDO releases the lock when a holder process dies). */
#if defined(__linux__) && !defined(MDB_USE_SYSV_SEM)
#define MDB_USE_SYSV_SEM 1
#endif
#include "mdb.c"
