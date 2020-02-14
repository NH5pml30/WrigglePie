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

        internal TreapNode((int y, int x, int no) data, TreapNode parent) :
            this(data.y, data.x, data.no, parent)
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

        internal TreapNode(List<(int y, int x, int no)> nodes, out TreapNode[] info)
        {
            var root = nodes.Min();
            nodes.Sort(((int y, int x, int no) left, (int y, int x, int no) right) => left.x - right.x);

            (y, x, no) = root;

            TreapNode[] lInfo = new TreapNode[nodes.Count];
            lInfo[no] = this;

            TreapNode getNode(int i)
            {
                if (lInfo[nodes[i].no] == null)
                    return lInfo[nodes[i].no] = new TreapNode(nodes[i], null);
                return lInfo[nodes[i].no];
            }

            if (nodes.Count > 1)
            {
                TreapNode
                    last = getNode(0),
                    prelast = null;
                for (int i = 1; i < nodes.Count; i++)
                {
                    while (last != null && nodes[i].y < last.y)
                    {
                        prelast = last;
                        last = last.parent;
                    }

                    TreapNode newNode = getNode(i);
                    if (last == null)
                        newNode.Append(prelast);
                    else
                        newNode.Append(last.Append(newNode));
                    last = newNode;
                }
            }

            info = lInfo;
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
            var nodes = new List<(int, int, int)>(n);

            for (int i = 0; i < n; i++)
            {
                string[] inp = Console.ReadLine().Split(' ');
                nodes.Add((Convert.ToInt32(inp[1]), Convert.ToInt32(inp[0]), i));
            }

            new TreapNode(nodes, out TreapNode[] info);
            Console.WriteLine("YES");
            foreach (TreapNode node in info)
                Console.WriteLine(
                        ((node.parent?.no ?? -1) + 1) + " " +
                        ((node.left?.no ?? -1) + 1) + " " +
                        ((node.right?.no ?? -1) + 1)
                    );
        }
    }
}
