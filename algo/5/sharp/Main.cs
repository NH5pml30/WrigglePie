#define CONTRACTS_FULL

using System;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics.Contracts;

class CircularListNode
{
    private CircularListNode() { }

    public static LinkedListNode<T> Next<T>(LinkedListNode<T> node)
    {
        return node.Next ?? node.List.First;
    }

    public static LinkedListNode<T> Previous<T>(LinkedListNode<T> node)
    {
        return node.Previous ?? node.List.Last;
    }
}

class Node
{
    protected Node(Node parent,
                   int siblingIndex,
                   List<int> keys,
                   List<Node> children)
    {
        this.parent = parent;
        this.siblingIndex = siblingIndex;
        this.keys = keys;
        this.children = children;
    }

    private Node parent;
    private int siblingIndex;
    private List<int> keys;
    private List<Node> children;

    private Node LeftSibling
    {
        get
        {
            return siblingIndex == 0 ?
                null : parent.children[siblingIndex - 1];
        }
    }

    private Node RightSibling
    {
        get
        {
            return siblingIndex == parent.children.Count - 1 ?
                null : parent.children[siblingIndex + 1];
        }
    }

    private void HandleOverflow(ref Node root)
    {
        Contract.Requires(keys.Count == 3);

        if (this == root)
        {
            Node
                newRoot = new Node(null, -1, keys.GetRange(1, 1), new List<Node>() { this, null }),
                right = new Node(newRoot, 1, keys.GetRange(2, 1), children.GetRange(2, 2));
            parent = newRoot;
            siblingIndex = 0;
            keys.RemoveRange(1, 1);
            children.RemoveRange(2, 2);

            newRoot.children[1] = right;

            root = newRoot;
        }
        else
        {
            parent.keys.Insert(siblingIndex + 1, keys[1]);
            keys.RemoveAt(1);
            children.GetRange(0, 2);
            parent.children.Insert(siblingIndex + 1, new Node(parent, siblingIndex + 1,
                                                              keys.GetRange(1, 1),
                                                              children.GetRange(2, 2)));
            keys.RemoveRange(1, 1);
            children.RemoveRange(2, 2);

            parent.Update(ref root);
        }
    }

    void HandleUnderflow(ref Node root)
    {
        Contract.Requires(keys.Count == 0);

        if (this == root)
        {
            children[0].parent = null;
            children[0].siblingIndex = -1;
            root = children[0];
        }
        else
        {
            int keyIndex = siblingIndex;
            Node sibling = LeftSibling;
            if (sibling == null)
            {
                keyIndex++;
                sibling = RightSibling;
            }

            int key = parent.keys[keyIndex];
            parent.keys.RemoveAt(keyIndex);
            parent.children.RemoveAt(siblingIndex);
            for (int i = siblingIndex; i < parent.children.Count; i++)
                parent.children[i].siblingIndex--;

            sibling.keys.Insert(0, key);
            sibling.children.InsertRange(0, children);
            children[0].parent = sibling;

            if (sibling.keys.Count == 3)
                sibling.HandleOverflow(ref root);
            else
                parent.Update(ref root);
        }
    }

    private void Update(ref Node root)
    {
        if (keys.Count == 0)
            HandleUnderflow(ref root);
        else if (keys.Count == 3)
            HandleOverflow(ref root);
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
