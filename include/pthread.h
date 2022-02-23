#pragma once

typedef struct pthread_mutexattr_t pthread_mutexattr_t;

typedef enum {
  PTHREAD_MUTEX_RECURSIVE,
} __pthread_mutex_type;

int pthread_mutexattr_init(pthread_mutexattr_t *mutex);
int pthread_mutexattr_destroy(pthread_mutexattr_t *mutex);
int pthread_mutexattr_settype(pthread_mutexattr_t *mutex,
                              __pthread_mutex_type type);

typedef struct pthread_mutex_t pthread_mutex_t;

int pthread_mutex_init(pthread_mutex_t *mutex, pthread_mutexattr_t *flags);
int pthread_mutex_destroy(pthread_mutex_t *mutex);
int pthread_mutex_lock(pthread_mutex_t *mutex);
int pthread_mutex_trylock(pthread_mutex_t *mutex);
int pthread_mutex_unlock(pthread_mutex_t *mutex);

typedef struct pthread_rwlock_t pthread_rwlock_t;

int pthread_rwlock_init(pthread_rwlock_t *lock, void *flags);
int pthread_rwlock_unlock(pthread_rwlock_t *lock);
int pthread_rwlock_rdlock(pthread_rwlock_t *lock);
int pthread_rwlock_wrlock(pthread_rwlock_t *lock);
int pthread_rwlock_destroy(pthread_rwlock_t *lock);
