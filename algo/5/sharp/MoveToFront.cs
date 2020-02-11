using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace MoveToFrontTask
{
    internal class TreapNode
    {
        internal int x;
        internal int y;
        internal int deltaX = 0;
        internal TreapNode left = null, right = null;

        internal TreapNode(int x, int y)
        {
            this.x = x;
            this.y = y;
        }

        internal TreapNode(Tuple<int, int> pair) :
            this(pair.Item1, pair.Item2)
        {
        }

        internal void ChangeDelta(int delta)
        {
            deltaX += delta;
        }

        internal void Split(int key, out TreapNode T1, out TreapNode T2, int accumDelta = 0)
        {
            TreapNode l = null, r = null;
            if (key > x + accumDelta + deltaX)
            {
                right?.Split(key, out l, out r, accumDelta + deltaX);
                right = l;
                if (r != null)
                    r.deltaX += deltaX;
                T1 = this;
                T2 = r;
            }
            else
            {
                left?.Split(key, out l, out r, accumDelta + deltaX);
                left = r;
                if (l != null)
                    l.deltaX += deltaX;
                T1 = l;
                T2 = this;
            }
        }

        internal static TreapNode Merge(TreapNode v1, TreapNode v2)
        {
            if (v1 == null)
                return v2;
            if (v2 == null)
                return v1;

            if (v1.x + v1.deltaX > v2.x + v2.deltaX)
                (v1, v2) = (v2, v1);

            bool isLeftParent = v1.y < v2.y;
            (TreapNode parent, TreapNode other) = isLeftParent ? (v1, v2) : (v2, v1);
            ref TreapNode place = ref (isLeftParent ? ref v1.right : ref v2.left);

            other.deltaX -= parent.deltaX;
            place = Merge(place, other);
            return parent;
        }

        public void RecursiveTraverse(Action<TreapNode, int> action, int accumDelta = 0)
        {
            left?.RecursiveTraverse(action, accumDelta + deltaX);
            action(this, accumDelta);
            right?.RecursiveTraverse(action, accumDelta + deltaX);
        }

        public TreapNode(int n) : this(0, 0)
        {
            TreapNode last = this;
            for (int i = 1; i < n; i++)
            {
                last.right = new TreapNode(i, i);
                last = last.right;
            }
        }
    }

    class EntryPoint
    {
        static void OutputTable(TreapNode treap, int stride, int offset)
        {
            Console.CursorLeft = 0;
            Console.CursorTop = offset * (stride + 1) * 2;
            for (int i = 0; i < stride; i++)
            {
                Console.WriteLine(
                    new StringBuilder(2 * stride)
                        .Insert(0, " |", stride)
                        .ToString()
                    );
                Console.WriteLine(
                    new StringBuilder(2 * stride)
                        .Insert(0, "-+", stride)
                        .ToString()
                    );
            }
            treap.RecursiveTraverse((TreapNode node, int accumDelta) =>
            {
                // Console.Write((node.y + 1).ToString() + " ");
                Console.CursorLeft = (node.x + node.deltaX + accumDelta) * 2;
                Console.CursorTop = offset * (stride + 1) * 2 + node.y * 2;
                Console.Write('*');
            });
            // Console.WriteLine();
        }

        static void Work()
        {
            string[] inp = Console.ReadLine().Split(' ');
            int n = Convert.ToInt32(inp[0]), m = Convert.ToInt32(inp[1]);

            TreapNode treap = new TreapNode(n);
            treap.RecursiveTraverse((TreapNode lN, int lA) => { });
            for (int i = 0; i < m; i++)
            {
                inp = Console.ReadLine().Split(' ');
                int l = Convert.ToInt32(inp[0]) - 1, r = Convert.ToInt32(inp[1]) - 1;
                treap.Split(l, out TreapNode left, out TreapNode middle);
                middle.Split(r + 1, out middle, out TreapNode right);

                left?.ChangeDelta(r - l + 1);
                right = TreapNode.Merge(left, right);
                middle.ChangeDelta(-l);
                treap = TreapNode.Merge(middle, right);
            }

            treap.RecursiveTraverse((TreapNode node, int accumDelta) =>
            {
                Console.Write((node.y + 1).ToString() + " ");
            });
            Console.WriteLine();
        }

        static void Main(string[] args)
        {
            Thread T = new Thread(Work, (1 << 20) * 15);
            T.Start();
            T.Join();
        }
    }
}
