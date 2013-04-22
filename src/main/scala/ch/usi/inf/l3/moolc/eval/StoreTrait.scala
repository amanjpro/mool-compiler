/*
 * Copyright (c) <2012-2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */


package ch.usi.inf.l3.moolc.evaluator

import _root_.ch.usi.inf.l3.moolc.ast._


trait StoreTrait {
  def newStore(env: Map[Var, PEValue]): StoreTrait
  def addEnv(env: Map[Var, PEValue]): Unit
  def get(v: Var): Option[PEValue]
  def add(v: Var, p: PEValue): Unit
  def remove(v: Var): Unit
  def isCT(v: Var): Boolean
  def isRT(v: Var): Boolean
  def cloneStore(): StoreTrait
}