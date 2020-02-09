using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TreapTask
{
    internal class TreapNode
    {
        internal int x;
        internal int y;
        internal int deltaX = 0;
        internal TreapNode parent = null;
        internal TreapNode left = null, right = null;

        internal TreapNode(int x, int y, TreapNode parent)
        {
            this.x = x;
            this.y = y;
            parent?.Append(this);
        }

        internal TreapNode(Tuple<int, int> pair, TreapNode parent) :
            this(pair.Item1, pair.Item2, parent)
        {
        }

        internal void ChangeDelta(int delta)
        {
            deltaX += delta;
        }

        internal TreapNode Append(TreapNode node)
        {
            node.parent = this;
            TreapNode save;
            if (node.x < x)
            {
                if (left != null)
                    left.parent = null;
                save = left;
                left = node;
            }
            else
            {
                if (right != null)
                    right.parent = null;
                save = right;
                right = node;
            }
            return save;
        }

        internal void Split(int key, out TreapNode T1, out TreapNode T2, int accumDelta = 0)
        {
            TreapNode l = null, r = null;
            if (key > x + accumDelta + deltaX)
            {
                right?.Split(key, out l, out r, accumDelta + deltaX);
                right = l;
                T1 = this;
                T2 = r;
            }
            else
            {
                left?.Split(key, out l, out r, accumDelta + deltaX);
                left = r;
                T1 = l;
                T2 = this;
            }
        }

        internal TreapNode Merge(TreapNode other)
        {
            if (other == null)
                return this;
            if (y > other.y)
            {
                right = right.Merge(other);
                return this;
            }
            else
            {
                other.left = Merge(other.left);
                return other;
            }
        }

        public TreapNode(List<Tuple<int, int>> nodes)
        {
            nodes.Sort(delegate (Tuple<int, int> left, Tuple<int, int> right)
            {
                return left.Item1 - right.Item1;
            });

            x = nodes[0].Item1;
            y = nodes[0].Item2;

            TreapNode last = this, lastChild = null;
            for (int i = 1; i < nodes.Count; i++)
            {
                while (last != null && nodes[i].Item2 < last.y)
                {
                    lastChild = last;
                    last = last.parent;
                }

                TreapNode newNode = new TreapNode(nodes[i], last);
                lastChild?.Append(newNode);
                last = newNode;
                lastChild = null;
            }
        }

        public void RecursiveTraverse(Action<TreapNode> action)
        {
            left?.RecursiveTraverse(action);
            action(this);
            right?.RecursiveTraverse(action);
        }
    }

    class EntryPoint
    {
        static void Main(string[] args)
        {
            string[] inp = Console.ReadLine().Split(' ');
            int n = Convert.ToInt32(inp[0]), m = Convert.ToInt32(inp[1]);
            List<Tuple<int, int>> nodes = new List<Tuple<int, int>>(n);
            for (int i = 0; i < n; i++)
                nodes.Add(new Tuple<int, int>(i, i));

            TreapNode treap = new TreapNode(nodes);
            for (int i = 0; i < m; i++)
            {
                inp = Console.ReadLine().Split(' ');
                int l = Convert.ToInt32(inp[0]) - 1, r = Convert.ToInt32(inp[1]) - 1;
                treap.Split(l, out TreapNode left, out TreapNode middle);
                middle.Split(r + 1, out middle, out TreapNode right);

                left.ChangeDelta(r - l + 1);
                right = left.Merge(right);
                middle.ChangeDelta(-l);
                treap = middle.Merge(right);
            }

            treap.RecursiveTraverse((TreapNode node) =>
            {
                Console.Write((node.y + 1).ToString() + " ");
            });
            Console.WriteLine();
        }
    }
}
