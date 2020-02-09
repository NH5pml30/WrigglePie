using System;
using System.Collections.Generic;
using System.Linq;

interface IBst<T> where T : IComparable
{
    void Insert(T x);
    void Delete(T x);
    bool Exists(T x);
    bool Next(T x, out T key);
    bool Prev(T x, out T key);
}

class Tree<T> : IBst<T> where T : IComparable
{
    private class Node
    {
        internal Node parent = null;
        internal int siblingIndex = -1;
        internal List<T> keys;
        internal List<Node> children;
        internal Tree<T> enclosing;

        internal Node(Tree<T> enclosing,
                      List<T> keys,
                      List<Node> children)
        {
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

        private void NextNode(T x, ref T key, ref Node node)
        {
            int childAt = keys.BinarySearch(x), keyAt;
            if (childAt >= 0)
                keyAt = ++childAt;
            else
                keyAt = (childAt = ~childAt);

            if (keyAt >= 0 && keyAt < keys.Count &&
                (key.Equals(x) || keys[keyAt].CompareTo(key) < 0))
            {
                node = this;
                key = keys[keyAt];
            }

            if (children.Any())
                children[childAt].NextNode(x, ref key, ref node);
        }

        internal Node NextNode(T x, out T key)
        {
            key = x;
            Node node = null;
            NextNode(x, ref key, ref node);
            return node;
        }

        private void PrevNode(T x, ref T key, ref Node node)
        {
            int childAt = keys.BinarySearch(x), keyAt;
            if (childAt >= 0)
                keyAt = childAt - 1;
            else
                keyAt = (childAt = ~childAt) - 1;

            if (keyAt >= 0 && keyAt < keys.Count &&
                (key.Equals(x) || keys[keyAt].CompareTo(key) > 0))
            {
                node = this;
                key = keys[keyAt];
            }

            if (children.Any())
                children[childAt].PrevNode(x, ref key, ref node);
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
            if (!children.Any())
                return this;
            else
            {
                int at = keys.BinarySearch(x);
                if (at >= 0)
                    return this;
                return children[~at].Find(x);
            }
        }

        internal T Split(out Node right)
        {
            bool isLeaf = !children.Any();
            right = new Node(
                enclosing,
                keys.GetRange(2, 1),
                isLeaf ?
                    new List<Node>() :
                    children.GetRange(2, 2)
                );
            T res = keys[1];
            keys.RemoveRange(1, 2);
            if (!isLeaf)
                children.RemoveRange(2, 2);
            return res;
        }

        internal void Append(int keyAt, T key, bool isRightNode, Node node)
        {
            keys.Insert(keyAt, key);
            int nodeAt = keyAt + (isRightNode ? 1 : 0);
            children.Insert(nodeAt, node);
            node.parent = this;
            node.siblingIndex = nodeAt;
            for (int index = nodeAt + 1; index < children.Count; index++)
                children[index].siblingIndex++;
        }

        internal T Remove(int keyAt, bool isRightNode)
        {
            T key = keys[keyAt];
            keys.RemoveAt(keyAt);

            int childAt = keyAt + (isRightNode ? 1 : 0);
            children[childAt].parent = null;
            children[childAt].siblingIndex = -1;
            children.RemoveAt(childAt);

            for (int i = childAt; i < children.Count; i++)
                children[i].siblingIndex--;

            return key;
        }

        internal void Insert(T x)
        {
            if (!children.Any())
            {
                keys.Add(x);
                keys.Sort();
                enclosing.Update(this);
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
                    enclosing.Update(this);
                }
            }
            else
            {
                int at = keys.BinarySearch(x);
                if (at >= 0)
                {
                    Node next = NextNode(x, out T key);
                    next.Delete(x);
                    enclosing.Update(next);
                    Node place = enclosing.Root.Find(x);
                    place.keys[place.keys.IndexOf(x)] = key;
                }
                else
                    children[~at].Delete(x);
            }
        }
    }

    private Node Root;

    public Tree()
    {
        Root = new Node(this, new List<T>(), new List<Node>());
    }


    private void HandleOverflow(Node at)
    {
        T key = at.Split(out Node right);

        if (ReferenceEquals(at, Root))
            Root = new Node(this, new List<T>() { key }, new List<Node>() { at, right });
        else
        {
            at.parent.Append(at.siblingIndex, key, true, right);
            Update(at.parent);
        }
    }

    private Node MergeWithSibling(Node self)
    {
        Node sibling;
        if (self.siblingIndex == 0)
        {
            sibling = self.parent.children[1];
            T key = self.parent.Remove(0, false);
            if (self.children.Any())
                sibling.Append(0, key, false, self.children[0]);
            else
                sibling.keys.Insert(0, key);
        }
        else
        {
            sibling = self.parent.children[self.siblingIndex - 1];
            T key = self.parent.Remove(self.siblingIndex - 1, true);
            if (self.children.Any())
                sibling.Append(sibling.keys.Count, key, true, self.children[0]);
            else
                sibling.keys.Insert(sibling.keys.Count, key);
        }
        self.children.Clear();
        self.keys.Clear();

        return sibling;
    }

    private void HandleUnderflow(Node at)
    {
        if (ReferenceEquals(at, Root))
        {
            if (at.children.Any())
            {
                Node newRoot = at.children[0];

                at.children.Clear();
                at.keys.Clear();

                newRoot.parent = null;
                newRoot.siblingIndex = -1;
                Root = newRoot;
            }
        }
        else
        {
            Node sibling = MergeWithSibling(at);

            if (sibling.keys.Count == 3)
                HandleOverflow(sibling);
            else
                Update(sibling.parent);
        }
    }

    private void Update(Node at)
    {
        if (at.keys.Count == 0)
            HandleUnderflow(at);
        else if (at.keys.Count == 3)
            HandleOverflow(at);
    }

    public void Insert(T x) => Root.Insert(x);

    public void Delete(T x) => Root.Delete(x);

    public bool Exists(T x)
    {
        Node node = Root.Find(x);
        return node.keys.Contains(x);
    }

    public bool Next(T x, out T key) => Root.NextNode(x, out key) != null;

    public bool Prev(T x, out T key) => Root.PrevNode(x, out key) != null;
}


namespace sharp
{
    class EntryPoint
    {
        static void Main(string[] args)
        {
            Tree<int> tree = new Tree<int>();

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
