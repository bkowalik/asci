package com.asci

case class Env(private val env: Map[String, Expr]) {
  def this() = this(Map.empty)

  def insert(name: String, expr: Expr): Env = copy(env = env.updated(name, expr))

  def remove(name: String): Env = copy(env = env - name)

  def get(name: String): Option[Expr] = env.get(name)
}
