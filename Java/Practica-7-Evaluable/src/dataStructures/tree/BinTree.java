/* *
 * Estructuras de Datos.
 * 2.º Grado en Ingeniería Informática, del Software y Computadores. UMA.
 *
 * Práctica evaluable 7. Enero 2021.
 *
 * Apellidos, Nombre: Rey Leyva, Alejandro
 * Grupo: Matemáticas + Infrormática
 */

package dataStructures.tree;

import dataStructures.list.*;


public class BinTree<T extends Comparable<? super T>> {

    private static class Tree<E> {
        private E elem;
        private Tree<E> left;
        private Tree<E> right;

        public Tree(E e, Tree<E> l, Tree<E> r) {
            elem = e;
            left = l;
            right = r;
        }
    }

    private Tree<T> root;

    public BinTree() {
        root = null;
    }

    public BinTree(T x) {
        root = new Tree<>(x, null, null);
    }

    public BinTree(T x, BinTree<T> l, BinTree<T> r) {
        root = new Tree<>(x, l.root, r.root);
    }

    public boolean isEmpty() {
        return root == null;
    }

    /**
     * Returns representation of tree as a String.
     */
    @Override
    public String toString() {
        return getClass().getSimpleName() + "(" + toStringRec(this.root) + ")";
    }

    private static String toStringRec(Tree<?> tree) {
        return tree == null ? "null" : "Node<" + toStringRec(tree.left) + ","
                + tree.elem + "," + toStringRec(tree.right)
                + ">";
    }

    /**
     * Returns a String with the representation of tree in DOT (graphviz).
     */
    public String toDot(String treeName) {
        final StringBuffer sb = new StringBuffer();
        sb.append(String.format("digraph %s {\n", treeName));
        sb.append("node [fontname=\"Arial\", fontcolor=red, shape=circle, style=filled, color=\"#66B268\", fillcolor=\"#AFF4AF\" ];\n");
        sb.append("edge [color = \"#0070BF\"];\n");
        toDotRec(root, sb);
        sb.append("}");
        return sb.toString();
    }

    private void toDotRec(Tree<T> current, StringBuffer sb) {
        if (current != null) {
            final int currentId = System.identityHashCode(current);
            sb.append(String.format("%d [label=\"%s\"];\n", currentId, current.elem));
            if (!isLeaf(current)) {
                processChild(current.left, sb, currentId);
                processChild(current.right, sb, currentId);
            }
        }
    }

    private static <T extends Comparable<? super T>> boolean isLeaf(Tree<T> current) {
        return current != null && current.left == null && current.right == null;
    }

    private void processChild(Tree<T> child, StringBuffer sb, int parentId) {
        if (child == null) {
            sb.append(String.format("l%d [style=invis];\n", parentId));
            sb.append(String.format("%d -> l%d;\n", parentId, parentId));
        } else {
            sb.append(String.format("%d -> %d;\n", parentId, System.identityHashCode(child)));
            toDotRec(child, sb);
        }
    }

    // Ejercicio 1

    public T maximum() { return maximumRec(root); }

    private T maximumRec(Tree<T> node) {
        // Si el arbol es vacío
        if (node == null)
            throw new BinTreeException("Árbol vacío");
        T elem = node.elem;
        // Si es un nodo hoja
        if (node.left == null && node.right == null)
            return elem;
            // Si la rama izq es null
        else if (node.left == null) {
            T maxRight = maximumRec(node.right);
            if (elem.compareTo(maxRight) > 0)
                return elem;
            else
                return maxRight;
        }
        // Si la rama izq es null
        else if (node.right == null) {
            T maxLeft = maximumRec(node.left);
            if (elem.compareTo(maxLeft) > 0)
                return elem;
            else
                return maxLeft;
        }
        // Si tiene dos ramas
        else {
            T maxLeft = maximumRec(node.left);
            T maxRight = maximumRec(node.right);
            if (elem.compareTo(maxLeft) > 0 && elem.compareTo(maxRight) > 0)
                return elem;
            else if (maxLeft.compareTo(elem) > 0 && maxLeft.compareTo(maxRight) > 0)
                return maxLeft;
            else
                return maxRight;
        }
    }

    // Ejercicio 2

    public int numBranches() {
        // Si el arbol es vacío
        if (root == null)
            throw new BinTreeException("Árbol vacío");

        // Si el árbol es un solo nodo
        if (root.left == null && root.right == null)
            return 0;

            // Si la rama izq es null
        else if (root.left == null)
            return numBranchesRec(root.right);
            // Si la rama dcha es null
        else if (root.right == null)
            return numBranchesRec(root.left);
            // Si tiene dos ramas
        else
            return numBranchesRec(root.left) + numBranchesRec(root.right);

    }

    private int numBranchesRec(Tree<T> node) {
        T elem = node.elem;

        if (node.left == null && node.right == null)
            return 1;
        else if (node.right == null)
            return numBranchesRec(node.left);
        else if (node.left == null)
            return numBranchesRec(node.right);
        else
            return numBranchesRec(node.left) + numBranchesRec(node.right);
    }

    // Ejercicio 3

    public List<T> atLevel (int i) {
        int level = 0;
        // Si el index está fuera de rango
        if (i < 0)
            return null;
        else
            return atLevelRec(i, root);
    }


    private List<T> atLevelRec(int i, Tree<T> node) {
        List <T> res = new LinkedList<>();
        List <T> resLeft = new LinkedList<>();
        List <T> resRight = new LinkedList<>();

        if (node != null) {
            if (i==0)
                res.append(node.elem);
            else {
                resLeft = atLevelRec(i-1, node.left);
                resRight = atLevelRec(i-1, node.right);
                for (T elem : resLeft) {
                    res.append(elem);
                }
                for (T elem : resRight) {
                    res.append(elem);
                }
            }
        }
        return res;
    }

    // Ejercicio 4

    public void rotateLeftAt(T x) {
        Tree<T> nodeX = search(x, root);
        Tree<T> nodeY;
        if (nodeX.right != null) {
            nodeY = nodeX.right;
            nodeX.left = nodeX;
            nodeX = nodeY;
            nodeX.right = nodeY.left;
        }
    }

    private Tree<T> search (T x, Tree<T> node) {
        if ( node == null )
            return null;

        T elem = node.elem;

        if (elem.equals(x))
            return node;
        else if (elem.compareTo(x) > 0) {
            if (node.left != null)
                return search(x, node.left);
            else
                return null;
        } else
            if (node.right != null)
                return search(x, node.right);
            else
                return null;
    }

    // Ejercicio 5

    public void decorate(T x) { decorateRec(x, root); }

    private void decorateRec(T x, Tree<T> node) {
        if (node != null) {
            if (node.left != null && node.right != null) {
                decorateRec(x, node.left);
                decorateRec(x, node.right);
            }
            if (node.left != null) {
                decorateRec(x, node.left);
                node.right = new Tree(x, null, null);
            }
            else if (node.right != null) {
                decorateRec(x, node.right);
                node.left = new Tree(x, null, null);
            }
            else {
                node.left = new Tree(x, null, null);
                node.right = new Tree(x, null, null);
            }
        }
    }
}
