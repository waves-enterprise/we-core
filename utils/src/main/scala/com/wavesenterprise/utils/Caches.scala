package com.wavesenterprise.utils

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}

object Caches {
  def cache[K <: AnyRef, V <: AnyRef](maximumSize: Int, loader: K => V): LoadingCache[K, V] =
    CacheBuilder
      .newBuilder()
      .maximumSize(maximumSize)
      .build(new CacheLoader[K, V] {
        override def load(key: K) = loader(key)
      })
}
