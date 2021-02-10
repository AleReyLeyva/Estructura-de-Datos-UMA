package dataStructures.tree.isomorphicTrees;

import dataStructures.list.ArrayList;
import dataStructures.list.List;

@SuppressWarnings("all")
public class IsomorphicDemo {

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

        BinTree<Character> tree2Char =
                new BinTree<>('H',
                        new BinTree<>('A',
                                new BinTree<>(),
                                new BinTree<>('S')),
                        new BinTree<>('K',
                                new BinTree<>('E'),
                                new BinTree<>('L')));

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

        // isomorphic
        for (int i = 0; i < trees.size(); i++) {
            int j = (i + 1) % trees.size();
            if (!trees.get(i).isomorphic(trees.get(i))) {
                System.err.println("ERROR: " + treeNames[i] + " should be isomorphic to itself");
                System.exit(-1);
            }
            if (trees.get(i).isomorphic(trees.get(j))) {
                System.err.println("ERROR: " + treeNames[i] + " and " + treeNames[i] + " should not be isomorphic");
                System.exit(-1);
            }
        }

        if (!tree2.isomorphic(tree2Char)) {
            System.err.println("ERROR: tree2 and tree2Char should be isomorphic");
            System.exit(-1);
        }

        if (!tree2Char.isomorphic(tree2)) {
            System.err.println("ERROR: tree2Char and tree2 should be isomorphic");
            System.exit(-1);
        }

        System.out.println("OK, DONE. GOOD JOB!");
    }
}
