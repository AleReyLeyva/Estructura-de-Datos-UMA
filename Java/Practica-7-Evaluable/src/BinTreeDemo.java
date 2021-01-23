import dataStructures.list.ArrayList;
import dataStructures.list.List;
import dataStructures.tree.BinTree;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;

public class BinTreeDemo {

    public static void main(String[] args) {

        List<BinTree<Integer>> trees = buildTrees();
        String[] treeNames = {"empty", "singleton", "tree1", "tree2", "tree3"};
        int[] rotateValues = {5, 5, 6, 8, 4};
        int[] decorateValues = {1, 4, 3, 2, 7};

        for (int i = 1; i < trees.size(); i++) {
            System.out.println("Tests for tree " + treeNames[i]);
            saveTreeToDot(treeNames[i], trees.get(i));
            testMaximum(trees.get(i));
            testNumBranches(trees.get(i));
            testAtLevel(trees.get(i));
            testRotate(treeNames[i], trees.get(i), rotateValues[i]);
            trees = buildTrees();
            testDecorate(treeNames[i], trees.get(i), decorateValues[i]);
            System.out.println("====================================================");
        }
    }

    private static <T extends Comparable<? super T>> void testMaximum(BinTree<T> tree) {
        try {
            System.out.println("maximum = " + tree.maximum());
        } catch (Exception e) {
            System.out.println("maximum = " + e.getMessage());
        }
    }

    private static <T extends Comparable<? super T>> void testNumBranches(BinTree<T> tree) {
        System.out.println("number of branches = " + tree.numBranches());
    }

    private static <T extends Comparable<? super T>> void testAtLevel(BinTree<T> tree) {
        System.out.println("show tree by levels:");
        int i = 0;
        List<T> level;
        do {
            level = tree.atLevel(i);
            System.out.println("\tat level " + i + " = " + level);
            i++;
        } while (!level.isEmpty());
    }

    private static <T extends Comparable<? super T>> void testRotate(String name, BinTree<T> tree, T x) {
        tree.rotateLeftAt(x);
        saveTreeToDot(name + "_RotatedAt_" + x, tree);
    }

    private static <T extends Comparable<T>> void testDecorate(String name, BinTree<T> tree, T x) {
        tree.decorate(x);
        saveTreeToDot(name + "_DecoratedWith_" + x, tree);
    }

    private static List<BinTree<Integer>> buildTrees() {
        BinTree<Integer> empty = new BinTree<>();

        BinTree<Integer> singleton = new BinTree<>(5);

        BinTree<Integer> tree1 = new BinTree<>(6, new BinTree<>(3), new BinTree<>(8));

        BinTree<Integer> tree2 =
                new BinTree<>(8,
                        new BinTree<>(27,
                                new BinTree<>(),
                                new BinTree<>(3)),
                        new BinTree<>(15,
                                new BinTree<>(21),
                                new BinTree<>(6)));
        BinTree<Integer> tree3;
        tree3 = new BinTree<>(16,
                new BinTree<>(8,
                        new BinTree<>(4,
                                new BinTree<>(),
                                new BinTree<>(6)),
                        new BinTree<>(12)),
                new BinTree<>(32,
                        new BinTree<>(24,
                                new BinTree<>(20),
                                new BinTree<>()),
                        new BinTree<>(64,
                                new BinTree<>(48),
                                new BinTree<>(82))));

        List<BinTree<Integer>> trees = new ArrayList<>();
        trees.append(empty);
        trees.append(singleton);
        trees.append(tree1);
        trees.append(tree2);
        trees.append(tree3);

        return trees;
    }

    private static <T extends Comparable<? super T>> void saveTreeToDot(String name, BinTree<T> tree) {
        try (PrintWriter pw = new PrintWriter(new File("dot/" + name + ".dot"))) {
            pw.println(tree.toDot(name));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}
