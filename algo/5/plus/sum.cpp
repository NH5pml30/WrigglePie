/* Kholiavin Nikolai, M3138 */
#include <algorithm>
#include <functional>
#include <iostream>
#include <stack>
#include <vector>

// #define NOMINMAX
// #include <windows.h>

#include <random>

class treap {
public:
	struct NODE {
		static std::default_random_engine eng;
		int x, y;
		NODE* left, * right;
		int min, max;
		long long sum = 0;

		static void Update(NODE* at) {
			if (at == nullptr) return;

			at->min = at->x;
			at->max = at->x;
			at->sum = at->x;
			if (at->left != nullptr) {
				at->min = std::min(at->min, at->left->min);
				at->max = std::max(at->max, at->left->max);
				at->sum += at->left->sum;
			}
			if (at->right != nullptr) {
				at->min = std::min(at->min, at->right->min);
				at->max = std::max(at->max, at->right->max);
				at->sum += at->right->sum;
			}
		}

		NODE(int x, NODE* left, NODE* right)
			: x(x), y(eng()), left(left), right(right) {
			Update(this);
		}

		static NODE* Merge(NODE* a, NODE* b) {
			NODE* res = nullptr;
			NODE** outer = &res;
			std::stack<NODE*> path;

			while (a != nullptr && b != nullptr) {
				if (a->y < b->y) {
					NODE* saved_a = a;
					*outer = a;
					path.push(saved_a);
					outer = &(saved_a->right);
					a = saved_a->right;
				}
				else {
					NODE* saved_b = b;
					*outer = b;
					path.push(saved_b);
					outer = &(saved_b->left);
					b = saved_b->left;
				}
			}
			*outer = (a == nullptr ? b : a);

			while (!path.empty()) {
				Update(path.top());
				path.pop();
			}

			return res;
		}

		// 0  1 2 3 4  5
		//|  | [x] |  |
		//|  |/   \|  |
		//| [x]   [x] |
		//| / \   / \ |
		static std::pair<NODE*, NODE*> SplitF(
			NODE* at, const std::function<int(NODE * node)>& where_to_split) {
			if (at == nullptr) return { nullptr, nullptr };

			struct LOCAL_DATA {
				NODE* t1 = nullptr, * t2 = nullptr;
				NODE* at;
				int split_at = -1;
				bool is_filled = false;

				LOCAL_DATA* prev;

				LOCAL_DATA(NODE* at, LOCAL_DATA* prev) : at(at), prev(prev) {}
			};

			std::stack<LOCAL_DATA*> stack;
			std::vector<LOCAL_DATA*> alloc;
			LOCAL_DATA root = LOCAL_DATA(nullptr, nullptr);
			alloc.push_back(new LOCAL_DATA(at, &root));
			stack.push(alloc.back());

			while (!stack.empty()) {
				LOCAL_DATA* data = stack.top();

				if (!data->is_filled) {
					int split_at = where_to_split(data->at);
					data->split_at = split_at;
					switch (split_at) {
					case 0:
						stack.pop();
						data->prev->t1 = nullptr;
						data->prev->t2 = data->at;
						data->prev->is_filled = true;
						break;
					case 1:
						alloc.push_back(new LOCAL_DATA(data->at->left, data));
						stack.push(alloc.back());
						break;
					case 2:
						stack.pop();
						data->prev->t1 = data->at->left;
						data->at->left = nullptr;
						data->prev->t2 = data->at;
						data->prev->is_filled = true;
						Update(data->prev->t1);
						Update(data->prev->t2);
						break;
					case 3:
						stack.pop();
						data->prev->t1 = data->at;
						data->prev->t2 = data->at->right;
						data->at->right = nullptr;
						data->prev->is_filled = true;
						Update(data->prev->t1);
						Update(data->prev->t2);
						break;
					case 4:
						alloc.push_back(new LOCAL_DATA(data->at->right, data));
						stack.push(alloc.back());
						break;
					case 5:
						stack.pop();
						data->prev->t1 = data->at;
						data->prev->t2 = nullptr;
						data->prev->is_filled = true;
						break;
					}
				}
				else {
					int split_at = data->split_at;
					LOCAL_DATA* prev = data->prev;

					switch (split_at) {
					case 1:
						data->at->left = data->t2;
						prev->t1 = data->t1;
						prev->t2 = data->at;
						Update(prev->t1);
						Update(prev->t2);
						break;
					case 4:
						data->at->right = data->t1;
						prev->t1 = data->at;
						prev->t2 = data->t2;
						Update(prev->t1);
						Update(prev->t2);
						break;
					default:
						throw "fuckich";
					}
					prev->is_filled = true;
					stack.pop();
				}
			}

			for (auto data : alloc) delete data;
			return { root.t1, root.t2 };
		}

		static std::pair<NODE*, NODE*> Split(NODE* at, int key) {
			return SplitF(at, [key](NODE* node) {
				if (node->x <= key) {
					if (node->right == nullptr) return 5;
					return 4;
				}
				else {
					if (node->left == nullptr) return 0;
					return 1;
				}
				});
		}

		/*
		static std::pair<NODE *, NODE *> SplitContinuous(NODE *at) {
			return SplitF(at, [](NODE *node, int) {
				if (node->left == nullptr || node->left->is_continuous) {
					if (node->left == nullptr ||
							node->x == node->left->max + node->left->delta_x + 1) {
						if (node->right == nullptr ||
								node->x == node->right->min + node->right->delta_x - 1) {
							if (node->right == nullptr || node->right->is_continuous)
								return 5;
							return 4;
						}
						return 3;
					}
					return 2;
				}
				return 1;
			});
		}
		*/

		static bool Contains(NODE* at, int x) {
			while (at != nullptr) {
				if (at->x < x) {
					at = at->right;
				}
				else if (at->x == x) {
					return true;
				}
				else {
					at = at->left;
				}
			}
			return false;
		}
	};

private:
	NODE* root = nullptr;
	std::vector<NODE*> nodes;

public:
	treap() {}

	int GetMin() { return root->min; }

	int GetMax() { return root->max; }

	long long GetSum() { return root->sum; }

	void Insert(int x) {
		if (!NODE::Contains(root, x)) {
			auto [l, r] = NODE::Split(root, x);
			nodes.push_back(new NODE(x, nullptr, nullptr));
			root = NODE::Merge(NODE::Merge(l, nodes.back()), r);
		}
	}

	long long GetSum(int l, int r) {
		auto [lt, mt1] = NODE::Split(root, l - 1);
		auto [mt, rt] = NODE::Split(mt1, r);
		long long res = 0;
		if (mt != nullptr) res = mt->sum;
		root = NODE::Merge(lt, NODE::Merge(mt, rt));
		return res;
	}

	void Traverse(std::function<void(NODE * at)> func) {
		std::stack<NODE*> path;
		path.push(root);

		while (!path.empty()) {
			auto cur = path.top();
			func(cur);
			path.pop();
			if (cur->left != nullptr)
				path.push(cur->left);
			if (cur->right != nullptr)
				path.push(cur->right);
		}
	}

	/*
	void Print(int x, int y, int h) {
		int w = GetMax() + 1;
		std::wstring even_a = L"-+", odd_a = L" |", even, odd;
		for (int i = 0; i < w; i++) even += even_a, odd += odd_a;
		HANDLE console = GetStdHandle(STD_OUTPUT_HANDLE);
		for (int i = 0; i < h; i++) {
			SetConsoleCursorPosition(console, {(short)x, (short)(y + i * 2)});
			WriteConsole(console, odd.data(), odd.size(), NULL, NULL);
			SetConsoleCursorPosition(console, {(short)x, (short)(y + i * 2 + 1)});
			WriteConsole(console, even.data(), even.size(), NULL, NULL);
		}
		std::wstring buf = L"*";
		Traverse([=](NODE *at, int accum_delta) {
			SetConsoleCursorPosition(
					console, {(short)(x + (at->x + at->delta_x + accum_delta) * 2),
										(short)(y + at->y * 2)});
			WriteConsole(console, buf.data(), 1, NULL, NULL);
		});
		SetConsoleCursorPosition(console, {(short)0, (short)y});
	}
	*/

	~treap() {
		for (auto n : nodes) delete n;
	}
};

std::default_random_engine treap::NODE::eng;

int main() {
	treap tree;

	int n;
	std::cin >> n;
	constexpr int mod = 1'000'000'000;
	int last_res = 0;
	char last_op = '+';
	for (int i = 0; i < n; i++) {
		char op;
		int arg1;
		std::cin >> op >> arg1;
		if (op == '+') {
			if (last_op == '+')
				tree.Insert(arg1);
			else
				tree.Insert((arg1 + last_res) % mod);
		}
		else {
			int arg2;
			std::cin >> arg2;
			long long res = tree.GetSum(arg1, arg2);
			std::cout << res << std::endl;
			while (res < 0) res += mod;
			last_res = (int)(res % mod);
		}
		last_op = op;
	}
	return 0;
}
