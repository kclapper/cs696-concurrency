package edu.coursera.concurrent;

import edu.rice.pcdp.Actor;

import static edu.rice.pcdp.PCDP.finish;

/**
 * An actor-based implementation of the Sieve of Eratosthenes.
 *
 * TODO Fill in the empty SieveActorActor actor class below and use it from
 * countPrimes to determine the number of primes <= limit.
 */
public final class SieveActor extends Sieve {
    /**
     * {@inheritDoc}
     *
     * TODO Use the SieveActorActor class to calculate the number of primes <=
     * limit in parallel. You might consider how you can model the Sieve of
     * Eratosthenes as a pipeline of actors, each corresponding to a single
     * prime number.
     */
    @Override
    public int countPrimes(final int limit) {
        if (limit < 2) {
            return 0;
        }

        final SieveActorActor actorChain = new SieveActorActor(2);

        finish(() -> {
            for (int i = 2; i <= limit; i++) {
                actorChain.send(i);
            }
        });

        return actorChain.count();
    }

    /**
     * An actor class that helps implement the Sieve of Eratosthenes in
     * parallel.
     */
    public static final class SieveActorActor extends Actor {
        /**
         * Process a single message sent to this actor.
         *
         * TODO complete this method.
         *
         * @param msg Received message
         */
        private final int prime;
        private SieveActorActor next;

        public SieveActorActor(int prime) {
            super();
            this.prime = prime;
        }

        @Override
        public void process(final Object msg) {
            int number = (int) msg;

            if (number % prime == 0) {
                return;
            }

            if (next == null) {
                next = new SieveActorActor(number);
            }

            next.send(msg);
        }

        public int count() {
            if (next == null) {
                return 1;
            }
            return 1 + next.count();
        }
    }
}
