package edu.coursera.concurrent;

import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * Wrapper class for two lock-based concurrent list implementations.
 */
public final class CoarseLists {
    /**
     * An implementation of the ListSet interface that uses Java locks to
     * protect against concurrent accesses.
     */
    public static final class CoarseList extends ListSet {
        private final ReentrantLock lock = new ReentrantLock();

        /**
         * Default constructor.
         */
        public CoarseList() {
            super();
        }

        @Override
        boolean add(final Integer object) {
            try {
                lock.lock();

                Entry pred = this.head;
                Entry curr = pred.next;

                while (curr.object.compareTo(object) < 0) {
                    pred = curr;
                    curr = curr.next;
                }

                if (object.equals(curr.object)) {
                    return false;
                } else {
                    final Entry entry = new Entry(object);
                    entry.next = curr;
                    pred.next = entry;
                    return true;
                }

            } finally {
                lock.unlock();
            }
        }

        @Override
        boolean remove(final Integer object) {
            try {
                lock.lock();

                Entry pred = this.head;
                Entry curr = pred.next;

                while (curr.object.compareTo(object) < 0) {
                    pred = curr;
                    curr = curr.next;
                }

                if (object.equals(curr.object)) {
                    pred.next = curr.next;
                    return true;
                } else {
                    return false;
                }

            } finally {
                lock.unlock();
            }
        }

        @Override
        boolean contains(final Integer object) {
            try {
                lock.lock();

                Entry pred = this.head;
                Entry curr = pred.next;

                while (curr.object.compareTo(object) < 0) {
                    pred = curr;
                    curr = curr.next;
                }
                return object.equals(curr.object);

            } finally {
                lock.unlock();
            }
        }
    }

    /**
     * An implementation of the ListSet interface that uses Java read-write
     * locks to protect against concurrent accesses.
     */
    public static final class RWCoarseList extends ListSet {
        private final ReentrantReadWriteLock lock = new ReentrantReadWriteLock();

        /**
         * Default constructor.
         */
        public RWCoarseList() {
            super();
        }

        @Override
        boolean add(final Integer object) {
            try {
                lock.writeLock().lock();

                Entry pred = this.head;
                Entry curr = pred.next;

                while (curr.object.compareTo(object) < 0) {
                    pred = curr;
                    curr = curr.next;
                }

                if (object.equals(curr.object)) {
                    return false;
                } else {
                    final Entry entry = new Entry(object);
                    entry.next = curr;
                    pred.next = entry;
                    return true;
                }

            } finally {
                lock.writeLock().unlock();
            }
        }

        @Override
        boolean remove(final Integer object) {
            try {
                lock.writeLock().lock();

                Entry pred = this.head;
                Entry curr = pred.next;

                while (curr.object.compareTo(object) < 0) {
                    pred = curr;
                    curr = curr.next;
                }

                if (object.equals(curr.object)) {
                    pred.next = curr.next;
                    return true;
                } else {
                    return false;
                }

            } finally {
                lock.writeLock().unlock();
            }
        }

        @Override
        boolean contains(final Integer object) {
            try {
                lock.readLock().lock();

                Entry pred = this.head;
                Entry curr = pred.next;

                while (curr.object.compareTo(object) < 0) {
                    pred = curr;
                    curr = curr.next;
                }
                return object.equals(curr.object);

            } finally {
                lock.readLock().unlock();
            }
        }
    }
}
