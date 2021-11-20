/**
 * Homework module.
 *
 * @author Nikolai Kholiavin M3238
 */
module info.kgeorgiy.ja.holyavin {
  requires transitive info.kgeorgiy.java.advanced.arrayset;
  requires transitive info.kgeorgiy.java.advanced.walk;
  requires transitive info.kgeorgiy.java.advanced.student;
  requires transitive info.kgeorgiy.java.advanced.implementor;
  requires transitive info.kgeorgiy.java.advanced.concurrent;
  requires transitive info.kgeorgiy.java.advanced.mapper;
  requires transitive info.kgeorgiy.java.advanced.crawler;
  requires transitive info.kgeorgiy.java.advanced.hello;

  requires java.compiler;
  requires java.rmi;
  requires jdk.httpserver;

  exports info.kgeorgiy.ja.holyavin.arrayset;
  exports info.kgeorgiy.ja.holyavin.walk;
  exports info.kgeorgiy.ja.holyavin.student;
  exports info.kgeorgiy.ja.holyavin.implementor;
  exports info.kgeorgiy.ja.holyavin.concurrent;
  exports info.kgeorgiy.ja.holyavin.crawler;
  exports info.kgeorgiy.ja.holyavin.hello;
  exports info.kgeorgiy.ja.holyavin.bank;
  exports info.kgeorgiy.ja.holyavin.statistics;
}
