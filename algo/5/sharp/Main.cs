#define CONTRACTS_FULL

using System;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics.Contracts;

interface IBst<T> where T : IComparable
{
    void Insert(T x);
    void Delete(T x);
    bool Exists(T x);
    T Next(T x);
    T Prev(T x);
}

class Tree<T> : IBst<T> where T : IComparable
{
    private class Node
    {
        internal Tree<T> enclosing;
        internal Node parent = null;
        internal int siblingIndex = -1;
        internal List<T> keys;
        internal List<Node> children;

        internal Node(Tree<T> enclosing,
                      List<T> keys,
                      List<Node> children)
        {
            Contract.Requires(
                (keys.Count == 2 || keys.Count == 3) &&
                (children.Count == 0 || children.Count == keys.Count + 1)
            );

            this.enclosing = enclosing;
            this.keys = keys;
            this.children = children;

            int index = 0;
            foreach (Node child in this.children)
            {
                child.parent = this;
                child.siblingIndex = index++;
            }
        }

        private T Split(out Node right)
        {
            right = new Node(enclosing, keys.GetRange(2, 1), children.GetRange(2, 2));
            T res = keys[1];
            keys.RemoveRange(1, 2);
            children.RemoveRange(2, 2);
            return res;
        }

        private void Append(int at, T key, Node node)
        {
            keys.Insert(at, key);
            children.Insert(at + 1, node);
            node.parent = this;
            node.siblingIndex = at + 1;
        }

        private T Remove(int keyAt, bool isRightNode, out Node node)
        {
            T key = keys[keyAt];
            keys.RemoveAt(keyAt);

            int childAt = keyAt + (isRightNode ? 1 : 0);
            children[childAt].parent = null;
            children[childAt].siblingIndex = -1;
            node = children[childAt];
            children.RemoveAt(childAt);

            for (int i = childAt; i < children.Count; i++)
                children[i].siblingIndex--;

            return key;
        }

        private void HandleOverflow()
        {
            T key = Split(out Node right);

            if (ReferenceEquals(this, enclosing.Root))
                enclosing.Root = new Node(enclosing, new List<T>() { key }, new List<Node>() { this, right });
            else
            {
                parent.Append(siblingIndex + 1, key, right);
                parent.Update();
            }
        }

        private Node MergeWithSibling()
        {
            int keyIndex = siblingIndex == 0 ? 0 : siblingIndex - 1;

            T key = parent.Remove(keyIndex, siblingIndex != 0, out Node sibling);
            sibling.Append(0, key, children[0]);
            children.Clear();
            keys.Clear();

            return sibling;
        }

        private void HandleUnderflow()
        {
            if (ReferenceEquals(this, enclosing.Root))
            {
                Node newRoot = children[0];

                children.Clear();
                keys.Clear();

                newRoot.parent = null;
                newRoot.siblingIndex = -1;
                enclosing.Root = newRoot;
            }
            else
            {
                Node sibling = MergeWithSibling();

                if (sibling.keys.Count == 3)
                    sibling.HandleOverflow();
                else
                    parent.Update();
            }
        }

        internal void Update()
        {
            if (keys.Count == 0)
                HandleUnderflow();
            else if (keys.Count == 3)
                HandleOverflow();
        }

        private void Traverse(Func<Node, T, bool> HandleLeafKey, Func<Node, T, int> HandleNodeKey)
        {
            if (!children.Any())
            {
                foreach (T key in keys)
                    if (HandleLeafKey(this, key))
                        break;
                return;
            }

            int index = 0;
            foreach (T key in keys)
            {
                int res = HandleNodeKey(this, key);
                if (res == -1)
                {
                    children[index].Traverse(HandleLeafKey, HandleNodeKey);
                    return;
                }
                if (res == 1)
                {
                    children[index + 1].Traverse(HandleLeafKey, HandleNodeKey);
                    return;
                }
                if (res != 0)
                    return;
                index++;
            }

            children.Last().Traverse(HandleLeafKey, HandleNodeKey);
        }

        private void Neighbor(bool isNext, T x, out T res, out Node node)
        {
            T lRes = x;
            Node lNode = null;

            /// > to leftleft
            bool func(Node self, T key)
            {
                if (isNext ?
                        key.CompareTo(x) > 0 :
                        key.CompareTo(x) < 0)
                {
                    if (lRes.Equals(x) || (isNext ?
                            key.CompareTo(lRes) < 0 :
                            key.CompareTo(lRes) > 0))
                    {
                        lNode = self;
                        lRes = key;
                    }
                    return true;
                }
                return false;
            }

            Traverse(
                func,
                (Node self, T key) => func(self, key) ? -1 : 0
            );

            res = lRes;
            node = lNode;
        }

        private Node NextNode(T x, out T key)
        {
            Neighbor(true, x, out T lKey, out Node node);
            key = lKey;
            return node;
        }

        private Node PrevNode(T x, out T key)
        {
            Neighbor(false, x, out T lKey, out Node node);
            key = lKey;
            return node;
        }

        internal T Next(T x)
        {
            NextNode(x, out T key);
            return key;
        }

        internal T Prev(T x)
        {
            PrevNode(x, out T key);
            return key;
        }

        private Node Find(T x)
        {
            Node lNode = null;

            Traverse(
                (Node self, T key) =>
                {
                    int comp = key.CompareTo(x);
                    if (comp == 0)
                        lNode = self;
                    return comp >= 0;
                },
                (Node self, T key) =>
                {
                    int comp = key.CompareTo(x);
                    if (comp == 0)
                        lNode = self;
                    return comp == 0 ? 2 : (comp > 0 ? -1 : 0);
                }
            );

            return lNode;
        }

        private void DeleteHelper(T x, bool found)
        {
            if (found)
            {
                if (!children.Any())
                {
                    keys.Remove(x);
                    Update();
                }
                else
                {
                    int at = keys.IndexOf(x);
                    
                }
            }
        }

        internal void Delete(T x)
        {
            ;/// Find(x, (Node node, bool found) => node.DeleteHelper(found, x));
        }
    }

    private Node Root;

    public Tree()
    {
        Root = new Node(this, new List<T>(), new List<Node>());
    }

    void IBst<T>.Insert(T x)
    {
    }

    void IBst<T>.Delete(T x)
    {
    }

    bool IBst<T>.Exists(T x)
    {
        throw new NotImplementedException();
    }

    T IBst<T>.Next(T x)
    {
        throw new NotImplementedException();
    }

    T IBst<T>.Prev(T x)
    {
        throw new NotImplementedException();
    }
}


namespace sharp
{
    class EntryPoint
    {
        static void Main(string[] args)
        {
        }
    }
}
