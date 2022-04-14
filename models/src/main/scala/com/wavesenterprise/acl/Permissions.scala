package com.wavesenterprise.acl

import cats.Monoid

import scala.collection.SortedSet

final case class Permissions private (value: SortedSet[PermissionOp]) {

  def copy(permissionOps: Seq[PermissionOp]): Permissions = {
    Permissions(permissionOps)
  }

  def toSeq: Seq[PermissionOp] = {
    value.toSeq
  }

  def filter(cond: PermissionOp => Boolean): Permissions = {
    Permissions(value.filter(cond))
  }

  def exists(cond: PermissionOp => Boolean): Boolean = value.exists(cond)

  def isEmpty: Boolean = {
    value.isEmpty
  }

  def contains(role: Role, onTimestamp: Long): Boolean =
    findRolePermission(role, onTimestamp)
      .fold(false)(_.isActive(onTimestamp))

  def containsWithoutDueDate(role: Role, onTimestamp: Long): Boolean =
    findRolePermission(role, onTimestamp)
      .fold(false)(_.isActiveWithoutDueDate)

  private def findRolePermission(role: Role, onTimestamp: Long): Option[PermissionOp] =
    value.find(p => p.role == role && p.timestamp <= onTimestamp)

  def active(onTimestamp: Long): Set[Role] =
    activeAsOps(onTimestamp).map(_.role)

  def activeAsOps(onTimestamp: Long): Set[PermissionOp] =
    value
      .filter(_.timestamp <= onTimestamp)
      .groupBy(_.role)
      .values
      .foldLeft(Set.empty[PermissionOp]) { (res, next) =>
        val first = next.firstKey
        if (first.isActive(onTimestamp))
          res + first
        else
          res
      }

  def lastActive(p: Role, onTimestamp: Long): Option[PermissionOp] =
    value
      .filter(_.timestamp <= onTimestamp)
      .collectFirst {
        case op if op.role == p =>
          if (op.isActive(onTimestamp))
            Some(op)
          else
            None
      }
      .flatten

  def combine(that: Permissions): Permissions = {
    Permissions(value | that.value) //todo: optimize set size by not adding duplicates?
  }
}

object Permissions {

  implicit val monoidInstance: Monoid[Permissions] = new Monoid[Permissions] {
    def empty: Permissions                                   = Permissions.empty
    def combine(x: Permissions, y: Permissions): Permissions = x.combine(y)
  }

  val ascendingOrdering: Ordering[PermissionOp] = Ordering.fromLessThan { (p1, p2) =>
    if (p1.timestamp != p2.timestamp)
      p1.timestamp < p2.timestamp
    else
      p1.role.byte < p2.role.byte
  }
  val descendingOrdering: Ordering[PermissionOp] = ascendingOrdering.reverse

  def empty: Permissions = apply(Seq.empty)

  def apply(permissionOps: SortedSet[PermissionOp]): Permissions = {
    new Permissions(permissionOps)
  }

  def apply(permissionOps: Seq[PermissionOp]): Permissions = {
    Permissions(SortedSet(permissionOps: _*)(descendingOrdering))
  }

}
