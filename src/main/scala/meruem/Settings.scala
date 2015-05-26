package meruem

import com.typesafe.config.{ConfigFactory, Config}

/**
 * Created by ybamelcash on 5/21/2015.
 */
case object Settings {
  val config = ConfigFactory.load()
  
  config.checkValid(ConfigFactory.defaultReference(), "language")
  
  val languageName = config.getString("language.name")
  val preloads = config.getStringList("language.preloaded")
}
