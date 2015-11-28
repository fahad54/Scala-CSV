package com.wage.test

import org.junit.runner.JUnitCore

object TestRunner extends App{
    val result = JUnitCore.runClasses(classOf[WageCalculatorTest])
    println(result.getFailures().toArray() mkString "\n")
}
