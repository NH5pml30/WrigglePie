#include <boost/program_options.hpp>
#include <atomic>
#include <iostream>
#include <thread>
#include <vector>
#include <memory>

namespace po = boost::program_options;

void thread_proc(std::atomic<int>& v, std::atomic<bool>& finish)
{
    while (!finish.load())
        v += 1;
}

int main(int argc, char* argv[])
{
    try
    {
        po::options_description desc("Allowed options");
        desc.add_options()
            ("threads,j", po::value<size_t>(), "set number of threads")
            ;

        po::variables_map vm;
        po::store(po::parse_command_line(argc, argv, desc), vm);
        po::notify(vm);

        size_t number_of_threads = 1;
        if (vm.count("threads") != 0)
            number_of_threads = vm["threads"].as<size_t>();

        for (;;)
        {
            std::cout << number_of_threads << " threads";

            std::atomic<int> val(0);
            std::atomic<bool> finished(false);

            std::vector<std::thread> threads;
            for (size_t i = 0; i != number_of_threads; ++i)
                threads.emplace_back(&thread_proc, std::ref(val), std::ref(finished));

            std::this_thread::sleep_for(std::chrono::seconds(2));
            finished.store(true);

            for (auto i = threads.rbegin(); i != threads.rend(); ++i)
            {
                auto& th = *i;
                th.join();
            }

            std::cout << ", " << val.load() << " iterations" << std::endl;
        }
    }
    catch (std::exception const& e)
    {
        std::cerr << e.what() << std::endl;
        return EXIT_FAILURE;
    }
}
