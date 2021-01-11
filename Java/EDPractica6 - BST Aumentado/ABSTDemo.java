public class ABSTDemo {

    public static void main(String[] args) {
        AugmentedBST<Integer> tree = new AugmentedBST<>();
        int[] xs = new int[]{20, 10, 30, 5, 15, 25, 35};
        // int[] xs = {60, 30, 20, 10, 5, 15, 90, 80, 85, 95};
        for (int x : xs) {
            tree.insert(x);
        }
        System.out.println(tree);
        String dot = tree.toDot("Ejemplo");
        System.out.println(dot);

        checkSelect(tree);

        Integer[] floors = {4, 5, 6, 9, 10, 17, 27, 50};
        checkFloor(tree, floors);

        Integer[] ceilings = {4, 5, 6, 9, 10, 17, 27, 50};
        checkCeiling(tree, ceilings);

        Integer[] keys = {4, 5, 6, 9, 10, 17, 27, 50};
        checkRank(tree, keys);

        checkSize(tree, keys);

    }

    private static <T extends Comparable<? super T>> void checkSize(AugmentedBST<T> tree, T[] keys) {
        for (T k : keys) {
            System.out.printf("size(%s,%s) = %d\n", keys[0], k, tree.size(keys[0], k));
        }
    }

    private static <T extends Comparable<? super T>> void checkSelect(AugmentedBST<T> abst) {
        for (int i = -1; i <= abst.size(); i++) {
            System.out.printf("select(%d) = %s\n", i, abst.select(i));
        }
    }

    private static <T extends Comparable<? super T>> void checkCeiling(AugmentedBST<T> abst, T[] ceilings) {
        for (T c : ceilings) {
            System.out.printf("ceiling(%s) = %s\n", c, abst.ceiling(c));
        }
    }

    private static <T extends Comparable<? super T>> void checkFloor(AugmentedBST<T> abst, T[] floors) {
        for (T f : floors) {
            System.out.printf("floor(%s) = %s\n", f, abst.floor(f));
        }
    }

    private static <T extends Comparable<? super T>> void checkRank(AugmentedBST<T> abst, T[] keys) {
        for (T k : keys) {
            System.out.printf("rank(%s) = %d\n", k, abst.rank(k));
        }
    }
}
