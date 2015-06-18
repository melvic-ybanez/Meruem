package meruem

import com.typesafe.config.{ConfigFactory, Config}

/**
 * Created by ybamelcash on 5/21/2015.
 */
case object Settings {
  val config = ConfigFactory.load()
  
  config.checkValid(ConfigFactory.defaultReference(), "language")
  config.checkValid(ConfigFactory.defaultReference(), "modules")
  config.checkValid(ConfigFactory.defaultReference(), "commands")
  
  val languageName = config.getString("language.name")
  
  val preloaded = config.getString("modules.preloaded")
  val mainFunction = config.getString("modules.main-function")
  val libLocation = config.getString("modules.lib-location")
  val fileExtendsion = config.getString("modules.file-extension")
  
  val exitCommand = config.getString("commands.exit")
}
