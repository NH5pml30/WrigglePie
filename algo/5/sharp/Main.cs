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

        internal Node(List<T> keys,
                      List<Node> children)
        {
            this.keys = keys;
            this.children = children;

            int index = 0;
            foreach (Node child in this.children)
            {
                child.parent = this;
                child.siblingIndex = index++;
            }
        }

        internal static void Traverse(Node node, Action<Node> leafAction, Func<Node, Node> nodeAction)
        {
            while (node != null)
            {
                if (!node.children.Any())
                {
                    leafAction(node);
                    break;
                }

                node = nodeAction(node);
            }
        }

        internal static Node Neighbor(Node atNode, bool isNext, T x, out T res)
        {
            void getAt(Node self, out int childAt, out int keyAt)
            {
                childAt = self.keys.BinarySearch(x);
                if (childAt >= 0)
                {
                    if (!isNext)
                        keyAt = childAt - 1;
                    else
                        keyAt = childAt + 1;
                    childAt += (isNext ? 1 : 0);
                }
                else
                {
                    childAt = ~childAt;
                    if (!isNext)
                        keyAt = childAt - 1;
                    else
                        keyAt = childAt;
                }
            }

            T lRes = x;
            Node node = null;

            Traverse(atNode,
                (Node self) =>
                {
                    getAt(self, out int childAt, out int keyAt);
                    if (keyAt >= 0 && keyAt < self.keys.Count &&
                        (lRes.Equals(x) || ((self.keys[keyAt].CompareTo(lRes) > 0) != isNext)))
                    {
                        node = self;
                        lRes = self.keys[keyAt];
                    }
                },
                (Node self) =>
                {
                    getAt(self, out int childAt, out int keyAt);

                    if (keyAt >= 0 && keyAt < self.keys.Count &&
                        (lRes.Equals(x) || ((self.keys[keyAt].CompareTo(lRes) > 0) != isNext)))
                    {
                        node = self;
                        lRes = self.keys[keyAt];
                    }
                    return self.children[childAt];
                });

            res = lRes;
            return node;
        }

        internal Node NextNode(T x, out T key) => Neighbor(this, true, x, out key);

        internal Node PrevNode(T x, out T key) => Neighbor(this, false, x, out key);

        internal Node Find(T x)
        {
            Node node = null;

            Traverse(this,
                (Node self) =>
                {
                    node = self;
                },
                (Node self) =>
                {
                    int at = self.keys.BinarySearch(x);
                    if (at >= 0)
                    {
                        node = self;
                        return null;
                    }

                    return self.children[~at];
                }
            );

            return node;
        }
    }

    private Node Root;

    public Tree()
    {
        Root = new Node(new List<T>(), new List<Node>());
    }

    private T SplitNode(Node self, out Node right)
    {
        bool isLeaf = !self.children.Any();
        right = new Node(
            self.keys.GetRange(2, 1),
            isLeaf ?
                new List<Node>() :
                self.children.GetRange(2, 2)
            );
        T res = self.keys[1];
        self.keys.RemoveRange(1, 2);
        if (!isLeaf)
            self.children.RemoveRange(2, 2);
        return res;
    }

    private void AppendToNode(Node self, int keyAt, T key, bool isRightNode, Node node)
    {
        self.keys.Insert(keyAt, key);
        int nodeAt = keyAt + (isRightNode ? 1 : 0);
        self.children.Insert(nodeAt, node);
        node.parent = self;
        node.siblingIndex = nodeAt;
        for (int index = nodeAt + 1; index < self.children.Count; index++)
            self.children[index].siblingIndex++;
    }

    private T RemoveFromNode(Node self, int keyAt, bool isRightNode)
    {
        T key = self.keys[keyAt];
        self.keys.RemoveAt(keyAt);

        int childAt = keyAt + (isRightNode ? 1 : 0);
        self.children[childAt].parent = null;
        self.children[childAt].siblingIndex = -1;
        self.children.RemoveAt(childAt);

        for (int i = childAt; i < self.children.Count; i++)
            self.children[i].siblingIndex--;

        return key;
    }

    private void HandleOverflow(Node at)
    {
        T key = SplitNode(at, out Node right);

        if (ReferenceEquals(at, Root))
            Root = new Node(new List<T>() { key }, new List<Node>() { at, right });
        else
        {
            AppendToNode(at.parent, at.siblingIndex, key, true, right);
            Update(at.parent);
        }
    }

    private Node MergeWithSibling(Node self)
    {
        Node sibling;
        if (self.siblingIndex == 0)
        {
            sibling = self.parent.children[1];
            T key = RemoveFromNode(self.parent, 0, false);
            if (self.children.Any())
                AppendToNode(sibling, 0, key, false, self.children[0]);
            else
                sibling.keys.Insert(0, key);
        }
        else
        {
            sibling = self.parent.children[self.siblingIndex - 1];
            T key = RemoveFromNode(self.parent, self.siblingIndex - 1, true);
            if (self.children.Any())
                AppendToNode(sibling, sibling.keys.Count, key, true, self.children[0]);
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

    public void Insert(T x)
    {
        Node place = Root.Find(x);
        if (!place.keys.Contains(x))
        {
            place.keys.Add(x);
            place.keys.Sort();
            Update(place);
        }
    }

    public void Delete(T x)
    {
        Node place = Root.Find(x);
        if (!place.keys.Contains(x))
            return;

        if (!place.children.Any())
        {
            place.keys.Remove(x);
            Update(place);
        }
        else
        {
            Node next = place.NextNode(x, out T key);
            next.keys.Remove(key);
            Update(next);

            place = Root.Find(x);
            place.keys[place.keys.IndexOf(x)] = key;
        }
    }

    public bool Exists(T x)
    {
        Node node = Root.Find(x);
        return node.keys.Contains(x);
    }

    public bool Next(T x, out T key)
    {
        return Root.NextNode(x, out key) != null;
    }

    public bool Prev(T x, out T key)
    {
        return Root.PrevNode(x, out key) != null;
    }
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
