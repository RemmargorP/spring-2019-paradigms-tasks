#include "tsqueue.h"

void threadsafe_queue_init(ThreadsafeQueue *q) {
    queue_init(&(q->queue));
    pthread_mutex_init(&(q->mutex), NULL);
    pthread_cond_init(&(q->cond_not_empty), NULL);
}

void threadsafe_queue_destroy(ThreadsafeQueue *q) {
    queue_destroy(&(q->queue));
    pthread_mutex_destroy(&(q->mutex));
    pthread_cond_destroy(&(q->cond_not_empty));
}

void threadsafe_queue_push(ThreadsafeQueue *q, void *data) {
    pthread_mutex_lock(&(q->mutex));
    queue_push(&(q->queue), data);
    pthread_cond_signal(&(q->cond_not_empty));
    pthread_mutex_unlock(&(q->mutex));
}

void *threadsafe_queue_wait_and_pop(ThreadsafeQueue *q) {
    pthread_mutex_lock(&(q->mutex));
    while (queue_empty(&(q->queue))) {
        pthread_cond_wait(&(q->cond_not_empty), &(q->mutex));
    }

    void *data = queue_pop(&(q->queue));

    pthread_mutex_unlock(&(q->mutex));

    return data;
}
