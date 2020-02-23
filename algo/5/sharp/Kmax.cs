using System;
using System.Collections.Generic;
using System.Linq;

namespace KmaxTask
{
    interface IBst<T> where T : IComparable
    {
        void Insert(T x);
        void Delete(T x);
        bool Exists(T x);
        bool Next(T x, out T key);
        bool Prev(T x, out T key);
    }

    internal class Node<T, NodeT>
        where T : IComparable
        where NodeT : Node<T, NodeT>
    {
        internal NodeT parent = null;
        internal int siblingIndex = -1;
        internal List<T> keys;
        internal List<NodeT> children;
        internal Tree<T, NodeT> enclosing;

        internal delegate NodeT NodeFactory(Tree<T, NodeT> enclosing, List<T> keys, List<NodeT> children);

        internal Node(Tree<T, NodeT> enclosing,
                      List<T> keys,
                      List<NodeT> children)
        {
            this.enclosing = enclosing;
            this.keys = keys;
            this.children = children;

            OnChanged();
        }

        internal virtual void OnChanged()
        {
            int index = 0;
            foreach (NodeT child in children)
            {
                child.parent = (NodeT)this;
                child.siblingIndex = index++;
            }
        }

        internal static void Walk(NodeT at, Func<NodeT, int> func)
        {
            while (at != null)
            {
                int childAt = func(at);
                if (childAt >= 0 && childAt < at.children.Count)
                    at = at.children[childAt];
                else
                    at = null;
            }
        }

        private static (T key, NodeT node) NextNode(NodeT at, T x)
        {
            T key = x;
            NodeT node = null;

            Walk(at,
                (NodeT self) =>
                {
                    int childAt = self.keys.BinarySearch(x), keyAt;
                    if (childAt >= 0)
                        keyAt = ++childAt;
                    else
                        keyAt = (childAt = ~childAt);

                    if (keyAt >= 0 && keyAt < self.keys.Count &&
                        (key.Equals(x) || self.keys[keyAt].CompareTo(key) < 0))
                    {
                        node = self;
                        key = self.keys[keyAt];
                    }

                    if (self.children.Any())
                        return childAt;
                    return -1;
                }
            );

            return (key, node);
        }

        internal NodeT NextNode(T x, out T key)
        {
            (T lKey, NodeT node) = NextNode((NodeT)this, x);
            key = lKey;
            return node;
        }

        private static (T key, NodeT node) PrevNode(NodeT at, T x)
        {
            T key = x;
            NodeT node = null;

            Walk(at,
                (NodeT self) =>
                {
                    int childAt = self.keys.BinarySearch(x), keyAt;
                    if (childAt >= 0)
                        keyAt = childAt - 1;
                    else
                        keyAt = (childAt = ~childAt) - 1;

                    if (keyAt >= 0 && keyAt < self.keys.Count &&
                        (key.Equals(x) || self.keys[keyAt].CompareTo(key) > 0))
                    {
                        node = self;
                        key = self.keys[keyAt];
                    }

                    if (self.children.Any())
                        return childAt;
                    return -1;
                }
            );

            return (key, node);
        }

        internal NodeT PrevNode(T x, out T key)
        {
            (T lKey, NodeT node) = PrevNode((NodeT)this, x);
            key = lKey;
            return node;
        }

        internal static NodeT Find(NodeT at, T x)
        {
            NodeT node = null;

            Walk(at,
                (NodeT self) =>
                {
                    if (!self.children.Any())
                    {
                        node = self;
                        return -1;
                    }

                    int childAt = self.keys.BinarySearch(x);
                    if (childAt >= 0)
                    {
                        node = self;
                        return -1;
                    }
                    return ~childAt;
                }
            );

            return node;
        }

        internal T Split(NodeFactory factory, out NodeT right)
        {
            bool isLeaf = !children.Any();
            right = factory(
                enclosing,
                keys.GetRange(2, 1),
                isLeaf ?
                    new List<NodeT>() :
                    children.GetRange(2, 2)
                );
            T res = keys[1];
            keys.RemoveRange(1, 2);
            if (!isLeaf)
                children.RemoveRange(2, 2);
            OnChanged();
            return res;
        }

        internal void Append(int keyAt, T key, bool isRightNode, NodeT node)
        {
            keys.Insert(keyAt, key);
            int nodeAt = keyAt + (isRightNode ? 1 : 0);
            children.Insert(nodeAt, node);
            node.parent = (NodeT)this;
            node.siblingIndex = nodeAt;
            OnChanged();
        }

        internal T Remove(int keyAt, bool isRightNode)
        {
            T key = keys[keyAt];
            keys.RemoveAt(keyAt);

            int childAt = keyAt + (isRightNode ? 1 : 0);
            children[childAt].parent = null;
            children[childAt].siblingIndex = -1;
            children.RemoveAt(childAt);
            OnChanged();
            return key;
        }

        internal void Insert(T x)
        {
            if (!children.Any())
            {
                if (!keys.Contains(x))
                {
                    keys.Add(x);
                    keys.Sort();
                    enclosing.Update((NodeT)this);
                }
            }
            else
            {
                int at = keys.BinarySearch(x);
                if (at < 0)
                    children[~at].Insert(x);
            }
        }

        internal void Delete(T x)
        {
            if (!children.Any())
            {
                if (keys.Contains(x))
                {
                    keys.Remove(x);
                    enclosing.Update((NodeT)this);
                }
            }
            else
            {
                int at = keys.BinarySearch(x);
                if (at >= 0)
                {
                    NodeT next = NextNode(x, out T key);
                    next.Delete(key);
                    Node<T, NodeT> place = Find(enclosing.Root, x);
                    place.keys[place.keys.IndexOf(x)] = key;
                }
                else
                    children[~at].Delete(x);
            }
        }
    }

    class Tree<T, NodeT> : IBst<T>
        where T : IComparable
        where NodeT : Node<T, NodeT>
    {
        internal NodeT Root;

        static NodeT DefaultFactory(Tree<T, NodeT> enclosing, List<T> keys, List<NodeT> children)
        {
            return (NodeT)new Node<T, NodeT>(enclosing, keys, children);
        }
        readonly Node<T, NodeT>.NodeFactory creator;

        public Tree(Node<T, NodeT>.NodeFactory factory)
        {
            creator = factory;
            Root = creator(this, new List<T>(), new List<NodeT>());
        }

        public Tree() : this(DefaultFactory)
        {
        }

        private void HandleOverflow(NodeT at)
        {
            T key = at.Split(creator, out NodeT right);

            if (ReferenceEquals(at, Root))
                Root = creator(this, new List<T>() { key }, new List<NodeT>() { at, right });
            else
            {
                at.parent.Append(at.siblingIndex, key, true, right);
                Update(at.parent);
            }
        }

        private NodeT MergeWithSibling(NodeT self)
        {
            NodeT sibling;
            if (self.siblingIndex == 0)
            {
                sibling = self.parent.children[1];
                T key = self.parent.Remove(0, false);
                if (self.children.Any())
                    sibling.Append(0, key, false, self.children[0]);
                else
                {
                    sibling.keys.Insert(0, key);
                    sibling.OnChanged();
                }
            }
            else
            {
                sibling = self.parent.children[self.siblingIndex - 1];
                T key = self.parent.Remove(self.siblingIndex - 1, true);
                if (self.children.Any())
                    sibling.Append(sibling.keys.Count, key, true, self.children[0]);
                else
                {
                    sibling.keys.Insert(sibling.keys.Count, key);
                    sibling.OnChanged();
                }
            }
            self.children.Clear();
            self.keys.Clear();

            sibling.parent.OnChanged();
            return sibling;
        }

        private void HandleUnderflow(NodeT at)
        {
            if (ReferenceEquals(at, Root))
            {
                if (at.children.Any())
                {
                    NodeT newRoot = at.children[0];

                    at.children.Clear();
                    at.keys.Clear();

                    newRoot.parent = null;
                    newRoot.siblingIndex = -1;
                    Root = newRoot;
                }
            }
            else
            {
                NodeT sibling = MergeWithSibling(at);

                if (sibling.keys.Count == 3)
                    HandleOverflow(sibling);
                else
                    Update(sibling.parent);
            }
        }

        internal void Update(NodeT at)
        {
            at.OnChanged();
            if (at.keys.Count == 0)
                HandleUnderflow(at);
            else if (at.keys.Count == 3)
                HandleOverflow(at);
            else if (at.parent != null)
                Update(at.parent);
        }

        public void Insert(T x) => Root.Insert(x);

        public void Delete(T x) => Root.Delete(x);

        public bool Exists(T x)
        {
            NodeT node = Node<T, NodeT>.Find(Root, x);
            return node.keys.Contains(x);
        }

        public bool Next(T x, out T key) => Root.NextNode(x, out key) != null;

        public bool Prev(T x, out T key) => Root.PrevNode(x, out key) != null;
    }

    interface IKmax<T> where T : IComparable
    {
        void Insert(T x);
        T Kmax(int k);
        void Delete(T x);
    }

    internal class NodeSize<T> : Node<T, NodeSize<T>>
        where T : IComparable
    {
        int subtreeSize;

        void UpdateSize()
        {
            subtreeSize = keys.Count;
            foreach (NodeSize<T> child in children)
                subtreeSize += child.subtreeSize;
        }

        internal override void OnChanged()
        {
            base.OnChanged();
            UpdateSize();
        }

        internal NodeSize(
                Tree<T, NodeSize<T>> enclosing,
                List<T> keys,
                List<NodeSize<T>> children
            ) : base(enclosing, keys, children)
        {
        }

        internal static T Kmax(NodeSize<T> at, int k)
        {
            T key = at.keys[0];
            k = at.subtreeSize - k + 1;

            Walk(at,
                (NodeSize<T> self) =>
                {
                    if (!self.children.Any())
                    {
                        key = self.keys[k - 1];
                        return -1;
                    }

                    int childAt = 0;
                    for (childAt = 0; childAt < self.children.Count - 1; childAt++)
                    {
                        if (k <= self.children[childAt].subtreeSize)
                            break;
                        k -= self.children[childAt].subtreeSize;
                        if (k == 1)
                        {
                            key = self.keys[childAt];
                            childAt = -1;
                            break;
                        }
                        k--;
                    }
                    return childAt;
                }
                );
            return key;
        }

        internal bool Check()
        {
            int size = keys.Count;
            foreach (NodeSize<T> child in children)
            {
                if (!child.Check())
                    return false;
                size += child.subtreeSize;
            }
            return size == subtreeSize;
        }
    }

    class KmaxTree<T> : Tree<T, NodeSize<T>>, IKmax<T>
        where T : IComparable
    {
        public KmaxTree() :
            base(
                    (Tree<T, NodeSize<T>> enclosing, List<T> keys, List<NodeSize<T>> children) =>
                        new NodeSize<T>(enclosing, keys, children)
                )
        {
        }

        public T Kmax(int k)
        {
            return NodeSize<T>.Kmax(Root, k);
        }

        public bool Check() => Root.Check();
    }

    class EntryPoint
    {
        static void Main(string[] args)
        {
            KmaxTree<int> tree = new KmaxTree<int>();

            int n = Convert.ToInt32(Console.ReadLine());
            for (int i = 0; i < n; i++)
            {
                string[] command = Console.ReadLine().Split(' ');
                int arg = Convert.ToInt32(command[1]);
                switch (command[0])
                {
                    case "1":
                    case "+1":
                        tree.Insert(arg);
                        break;
                    case "-1":
                        tree.Delete(arg);
                        break;
                    case "0":
                        Console.WriteLine(tree.Kmax(arg));
                        break;
                }
            }
        }
    }
}
