package meruem

import java.nio.file.Paths

import Constants._

import com.typesafe.config.ConfigFactory

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
  val libLocation = Paths.get(System.getenv("MERUEM_HOME")).resolve(config.getString("modules.lib-location")).toString
  val fileExtension = config.getString("modules.file-extension")
  
  val exitCommand = config.getString("commands.exit")
}
