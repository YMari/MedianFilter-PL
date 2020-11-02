import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import akka.actor.{Actor, ActorSystem, Props}
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


class MedianFilterActor extends Actor {
  var inputImg: BufferedImage = ImageIO.read(new File(getClass().getResource("/inputImg.png").toURI()))
  var outputImg: BufferedImage = new BufferedImage(inputImg.getWidth, inputImg.getHeight, BufferedImage.TYPE_INT_RGB)

  var SeqMFOutput = new FilterClass().SequentialMedianFilter(inputImg, outputImg, "outputImg1.png", 0, inputImg.getWidth, 0, inputImg.getHeight)
  var ParMFOutput = new FilterClass().ParallelMedianFilter(inputImg, outputImg, "outputImg2.png")

  def receive = {
    case "ClientSeq" => SeqMFOutput
    case "ClientPar" => ParMFOutput
    case _ => println("Nothing happened.")
  }

  println("Finalizing...")
  Thread.sleep(3000) // wait a few seconds before terminating to avoid errors
  println("Finished: Output Images and Report found in project folder.")
  MedianFilterApp.system.terminate()
}

object MedianFilterApp extends App {
  var system = ActorSystem("MedianFilterSystem")
  var medianActor = system.actorOf(Props[MedianFilterActor], name = "MedianFilterActor")

  medianActor ! "ClientSeq"
  medianActor ! "ClientPar"

}

class FilterClass() {
  def MedianFilter(inputImg: BufferedImage, outputImg: BufferedImage, path: String, i0: Int, i1: Int, j0: Int, j1: Int): Unit ={
    var winWidth = 10
    var winHeight = 10
    var edgex = (winWidth/2)
    var edgey = (winHeight/2)
    var window = Array.ofDim[Int](winWidth * winHeight)

    for (x <- i0 until i1) { //x from 0 to (image width - 1)
      for (y <- j0 until j1){ //y from 0 to (image height - 1)
        var i = 0
        for (fx <- 0 until winWidth) { //fx from j to window width
          for (fy <- 0 until winHeight) { //fy from k to window height
            try {
              window(i) = inputImg.getRGB(x + fx - edgex, y + fy - edgey);
            }
            catch { // ignore edges (avoid OutOfBoundsException)
              case _ => window(i) = inputImg.getRGB(fx, fy)
            }
            i += 1
          }
        }
        window.sortInPlace()
        outputImg.setRGB(x, y, window(winWidth * winHeight / 2))
      }
    }
    ImageIO.write(outputImg, "png", new File(path))
  }

  def SequentialMedianFilter(inputImg: BufferedImage, outputImg: BufferedImage, path: String, i0: Int, i1: Int, j0: Int, j1: Int): Unit = {
    var time_0 = System.nanoTime()

    println("Sequential Median Filter Actor called...")
    MedianFilter(inputImg, outputImg, path, i0, i1, j0, j1)

    var time_1 = System.nanoTime()
    var result = (time_1 - time_0)*(1/1E9)
    println("Sequential Median Filter took: " + f"$result%1.5f" + " seconds\n")
  }

  def ParallelMedianFilter(inputImg: BufferedImage, outputImg: BufferedImage, path: String): Unit = {
    var time_0 = System.nanoTime()

    println("Parallel Median Filter Actor called...")
    var par1 = Future {
      MedianFilter(inputImg, outputImg, path, 0, inputImg.getWidth/2, 0, inputImg.getHeight/2)
    }
    var par2 = Future {
      MedianFilter(inputImg, outputImg, path, inputImg.getWidth/2, inputImg.getWidth, 0, inputImg.getHeight/2)
    }
    var par3 = Future {
      MedianFilter(inputImg, outputImg, path, 0, inputImg.getWidth/2, inputImg.getHeight/2, inputImg.getHeight)
    }
    var par4 = Future {
      MedianFilter(inputImg, outputImg, path, inputImg.getWidth/2, inputImg.getWidth, inputImg.getHeight/2, inputImg.getHeight)
    }

    Await.ready(par4, Duration.Inf)

    var time_1 = System.nanoTime()
    var result = (time_1 - time_0)*(1/1E9)
    println("Parallel Median Filter took: " + f"$result%1.5f" + " seconds\n")
  }
}
