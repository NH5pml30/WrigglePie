using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TreapTask
{
    internal class TreapNode
    {
        internal int no, x, y;
        internal TreapNode left = null, right = null;
        internal TreapNode parent = null;

        internal TreapNode(int y, int x, int no, TreapNode parent)
        {
            this.no = no;
            this.x = x;
            this.y = y;
            parent?.Append(this);
        }

        internal TreapNode(Tuple<int, int, int> pair, TreapNode parent) :
            this(pair.Item1, pair.Item2, pair.Item3, parent)
        {
        }

        internal TreapNode Append(TreapNode child)
        {
            if (child == null)
                return null;

            child.parent = this;
            TreapNode save;
            if (child.x < x)
            {
                save = left;
                left = child;
            }
            else
            {
                save = right;
                right = child;
            }
            return save;
        }

        internal TreapNode(List<Tuple<int, int, int>> nodes)
        {
            Tuple<int, int, int> root = nodes.Min();
            nodes.Sort((Tuple<int, int, int> left, Tuple<int, int, int> right) => left.Item2 - right.Item2);

            (y, x, no) = root;
            TreapNode last = new TreapNode(nodes[0], null), prelast = null;
            for (int i = 1; i < nodes.Count; i++)
            {
                while (last != null && nodes[i].Item1 < last.y)
                {
                    prelast = last;
                    last = last.parent;
                }

                TreapNode newNode = nodes[i].Item3 == no ? this : new TreapNode(nodes[i], null);
                if (last == null)
                    newNode.Append(prelast);
                else
                    newNode.Append(last.Append(newNode));
                last = newNode;
            }
        }

        public void RecursiveTraverse(Action<int, int?, int?, int?> action)
        {
            left?.RecursiveTraverse(action);
            action(no, parent?.no, left?.no, right?.no);
            right?.RecursiveTraverse(action);
        }
    }

    class EntryPoint
    {
        static void Main(string[] args)
        {
            int n = Convert.ToInt32(Console.ReadLine());
            var nodes = new List<Tuple<int, int, int>>(n);

            for (int i = 0; i < n; i++)
            {
                string[] inp = Console.ReadLine().Split(' ');
                nodes.Add(Tuple.Create(Convert.ToInt32(inp[1]), Convert.ToInt32(inp[0]), i));
            }

            TreapNode treap = new TreapNode(nodes);
            Console.WriteLine("YES");
            treap.RecursiveTraverse((int no, int? parentNo, int? leftNo, int? rightNo) =>
            {
                nodes[no] = Tuple.Create((parentNo ?? -1) + 1, (leftNo ?? -1) + 1, (rightNo ?? -1) + 1);
            });
            foreach (var node in nodes)
                Console.WriteLine(node.Item1 + " " + node.Item2 + " " + node.Item3);
        }
    }
}
