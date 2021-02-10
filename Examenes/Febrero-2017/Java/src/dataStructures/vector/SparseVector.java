/******************************************************************************
 * Student's name:
 * Student's group:
 * Data Structures. Grado en Informática. UMA.
******************************************************************************/

package dataStructures.vector;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class SparseVector<T> implements Iterable<T> {

    protected interface Tree<T> {

        T get(int sz, int i);

        Tree<T> set(int sz, int i, T x);
    }

    // Unif Implementation

    protected static class Unif<T> implements Tree<T> {

        private T elem;

        public Unif(T e) {
            elem = e;
        }

        @Override
        public T get(int sz, int i) {
            return elem;
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
            if (sz == 1) {
                elem = x;
                return this;
            } else {
                if (i <= (sz/2) - 1) {
                    return (new Node<>(new Unif<>(elem).set(sz/2, i, x), new Unif<>(elem))).simplify();
                } else {
                    return (new Node<>(new Unif<>(elem), new Unif<>(elem).set(sz/2, i - sz/2, x))).simplify();
                }
            }
        }

        @Override
        public String toString() {
            return "Unif(" + elem + ")";
        }
    }

    // Node Implementation

    protected static class Node<T> implements Tree<T> {

        private Tree<T> left, right;

        public Node(Tree<T> l, Tree<T> r) {
            left = l;
            right = r;
        }

        @Override
        public T get(int sz, int i) {
            if (i <= (sz/2) - 1) {
                if (left instanceof Unif<?>) {
                    Unif<T> u = (Unif<T>) left;
                    return u.get(sz/2, i);
                } else {
                    Node<T> n = (Node<T>) left;
                    return n.get(sz/2, i);
                }
            } else {
                if (right instanceof Unif<?>) {
                    Unif<T> u = (Unif<T>) right;
                    return u.get(sz/2, i - sz/2);
                } else {
                    Node<T> n = (Node<T>) right;
                    return n.get(sz/2, i - sz/2);
                }
            }
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
            if (i <= (sz/2) - 1) {
                left = left.set(sz/2, i, x);
            } else {
                right = right.set(sz/2, i - sz/2, x);
            }
            return this.simplify();
        }

        protected Tree<T> simplify() {
            if (left instanceof Unif<?> && right instanceof Unif<?>) {
                Unif<T> u1 = (Unif<T>) left;
                Unif<T> u2 = (Unif<T>) right;
                if (u1.elem.equals(u2.elem)) {
                    return u1;
                } else {
                    return this;
                }
            } else {
                return this;
            }
        }

        @Override
        public String toString() {
            return "Node(" + left + ", " + right + ")";
        }
    }

    // SparseVector Implementation

    private int size;
    private Tree<T> root;

    public SparseVector(int n, T elem) {
        if (n < 0) {
            throw new VectorException("n must be positive");
        } else {
            size = (int) Math.pow(2, n);
            root = new Unif<>(elem);
        }
    }

    public int size() {
        return size;
    }

    public T get(int i) {
        if (i < 0 || i >= size) {
            throw new VectorException("Index Out of Bounds");
        } else {
            return root.get(size, i);
        }
    }

    public void set(int i, T x) {
        if (i < 0 || i >= size) {
            throw new VectorException("Index Out of Bounds");
        } else {
            root = root.set(size, i, x);
        }
    }

    @Override
    public Iterator<T> iterator() {
        return new SparseVectorIterator();
    }

    private class SparseVectorIterator implements Iterator<T> {
        private int i;

        public SparseVectorIterator() {
            i = 0;
        }

        public boolean hasNext() {
            return i < size;
        }

        public T next() {
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            i++;
            return root.get(size, i-1);
        }
    }

    @Override
    public String toString() {
        return "SparseVector(" + size + "," + root + ")";
    }
}
