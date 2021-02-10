package dataStructures.tree.trimmedTrees;

import dataStructures.list.ArrayList;
import dataStructures.list.List;

import java.io.FileNotFoundException;
import java.io.PrintWriter;

@SuppressWarnings("all")
public class TrimDemo {

    public static void main(String[] args) {

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

        BinTree<Integer> tree3 =
                new BinTree<>(9,
                        new BinTree<>(12,
                                new BinTree<>(),
                                new BinTree<>(23)),
                        new BinTree<>(6,
                                new BinTree<>(4,
                                        new BinTree<>(10),
                                        new BinTree<>()),
                                new BinTree<>(7)));

        BinTree<Integer> tree4 =
                new BinTree<>(16,
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
        trees.append(tree4);
        String[] treeNames = {"empty", "singleton", "tree1", "tree2", "tree3", "tree4"};

        // trim
        for (int i = 0; i < trees.size(); i++) {
            // to use dotplugin, create the "dot" directory and comment out the sentence below
            saveTreeToDot(treeNames[i], trees.get(i));
            System.out.println(treeNames[i] + ":\n" + trees.get(i));
            trees.get(i).trim();
            // to use dotplugin, create the "dot" directory and comment out the sentence below
            saveTreeToDot(treeNames[i] + "-trimmed", trees.get(i));
            System.out.println(treeNames[i] + "-trimmed:\n" + trees.get(i));
            System.out.println("===========================================");
        }
    }

    private static <T extends Comparable<? super T>> void saveTreeToDot(String name, BinTree<T> tree) {
        try (PrintWriter pw = new PrintWriter("dot/" + name + ".dot")) {
            pw.println(tree.toDot(name));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}
