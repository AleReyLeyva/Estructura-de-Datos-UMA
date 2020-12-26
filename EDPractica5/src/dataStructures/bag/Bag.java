package dataStructures.bag;

/**
 * Interface for the Bag ADT.
 *
 * @param <T> Type of elements in bag. Note that {@code T} could be
 *            {@code Comparable} in some implementations.
 */
public interface Bag<T> extends Iterable<T> {

    /**
     * Test the bag for emptiness.
     *
     * @return {@code true} if bag is empty, {@code false} otherwise.
     */
    boolean isEmpty();

    /**
     * Inserts a new occurrence in the bag.
     *
     * @param item the element to insert.
     */
    void insert(T item);

    /**
     * Returns the number of occurrences of {@code item} in the bag
     *
     * @param item the item to be counted
     * @return number of occurrences of {@code item}
     */
    int occurrences(T item);

    /**
     * Removes an occurrence of {@code item} from the bag
     *
     * @param item the item to remove
     */
    void delete(T item);

    /**
     * Copies the contents of {@code source} into the bag, so the two bags are
     * identical; i.e., previous contents of {@code this} bag are discarded.
     *
     * @param source the bag to be copied
     */
    void copyOf(Bag<T> source);
}
