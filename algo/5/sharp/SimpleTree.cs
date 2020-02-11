using System;

namespace SimpleTreeTask
{
    interface IBst<T> where T : IComparable
    {
        void Insert(T x);
        void Delete(T x);
        bool Exists(T x);
        bool Next(T x, out T key);
        bool Prev(T x, out T key);
    }

    class SimpleTree<T> : IBst<T> where T : IComparable
    {
        private class Node
        {
            private T key;
            private Node left = null, right = null;

            internal Node(T key)
            {
                this.key = key;
            }

            private void NextNode(T x, ref T resKey, ref Node node)
            {
                int comp = key.CompareTo(x);

                if (comp > 0)
                {
                    if (resKey.Equals(x) || key.CompareTo(resKey) < 0)
                    {
                        resKey = key;
                        node = this;
                    }
                    left?.NextNode(x, ref resKey, ref node);
                }
                else
                    right?.NextNode(x, ref resKey, ref node);
            }

            private void PrevNode(T x, ref T resKey, ref Node node)
            {
                int comp = key.CompareTo(x);

                if (comp < 0)
                {
                    if (resKey.Equals(x) || key.CompareTo(resKey) > 0)
                    {
                        resKey = key;
                        node = this;
                    }
                    right?.PrevNode(x, ref resKey, ref node);
                }
                else
                    left?.PrevNode(x, ref resKey, ref node);
            }

            internal Node NextNode(T x, out T key)
            {
                key = x;
                Node node = null;
                NextNode(x, ref key, ref node);
                return node;
            }

            internal Node PrevNode(T x, out T key)
            {
                key = x;
                Node node = null;
                PrevNode(x, ref key, ref node);
                return node;
            }

            internal Node Find(T x)
            {
                int comp = key.CompareTo(x);
                if (comp < 0)
                    return right?.Find(x);
                else if (comp > 0)
                    return left?.Find(x);
                return this;
            }

            internal void Insert(T x)
            {
                int comp = key.CompareTo(x);
                if (comp < 0)
                {
                    if (right != null)
                        right.Insert(x);
                    else
                        right = new Node(x);
                }
                else if (comp > 0)
                {
                    if (left != null)
                        left.Insert(x);
                    else
                        left = new Node(x);
                }
            }

            internal Node Delete(T x)
            {
                int comp = key.CompareTo(x);
                if (comp > 0)
                {
                    if (left != null)
                        left = left.Delete(x);
                    return this;
                }
                else if (comp < 0)
                {
                    if (right != null)
                        right = right.Delete(x);
                    return this;
                }

                if (right == null)
                {
                    Node save = left;
                    left = null;
                    return save;
                }
                else if (left == null)
                {
                    Node save = right;
                    right = null;
                    return save;
                }

                right.NextNode(key, out T lKey);
                right = right.Delete(lKey);
                key = lKey;
                return this;
            }
        }

        private Node root = null;

        public SimpleTree()
        {
        }

        public void Insert(T x)
        {
            if (root != null)
                root.Insert(x);
            else
                root = new Node(x);
        }

        public void Delete(T x)
        {
            root = root?.Delete(x);
        }

        public bool Exists(T x)
        {
            return root?.Find(x) != null;
        }

        public bool Next(T x, out T key)
        {
            if (root == null)
            {
                key = x;
                return false;
            }
            return root.NextNode(x, out key) != null;
        }

        public bool Prev(T x, out T key)
        {
            if (root == null)
            {
                key = x;
                return false;
            }
            return root.PrevNode(x, out key) != null;
        }
    }

    class EntryPoint
    {
        static void Main(string[] args)
        {
            SimpleTree<int> tree = new SimpleTree<int>();

            string s;
            while ((s = Console.ReadLine()) != null)
            {
                string[] command = s.Split(' ');
                int arg = Convert.ToInt32(command[1]), key;
                switch (command[0])
                {
                    case "insert":
                        tree.Insert(arg);
                        break;
                    case "delete":
                        tree.Delete(arg);
                        break;
                    case "exists":
                        Console.WriteLine(tree.Exists(arg).ToString().ToLower());
                        break;
                    case "next":
                        Console.WriteLine(tree.Next(arg, out key) ? key.ToString() : "none");
                        break;
                    case "prev":
                        Console.WriteLine(tree.Prev(arg, out key) ? key.ToString() : "none");
                        break;
                }
            }
        }
    }
}
