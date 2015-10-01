package meruem

import java.nio.file.Paths

import Constants._

import com.typesafe.config.ConfigFactory

/**
 * Created by ybamelcash on 5/21/2015.
 */
case object Settings {
  val config = ConfigFactory.load()
  
  config.checkValid(ConfigFactory.defaultReference(), "project")
  config.checkValid(ConfigFactory.defaultReference(), "modules")
  config.checkValid(ConfigFactory.defaultReference(), "commands")
  
  val languageName = config.getString("project.name")
  val version = config.getString("project.version")
  val projectType = config.getString("project.type")
  
  val preloaded = config.getString("modules.preloaded")
  val mainFunction = config.getString("modules.main-function")
  val libLocation = Paths.get(System.getenv("MERUEM_HOME")).resolve(config.getString("modules.lib-location")).toString
  val fileExtension = config.getString("modules.file-extension")
  
  val exitCommand = config.getString("commands.exit")
}
