import java.nio.file._
import java.time.LocalDate

object PathImplicits {
	implicit class Function1(path: Path) {
		def / (str: String): Path = path.resolve(str)
		def / (newPath: Path): Path = path.resolve(newPath)
	}

	implicit class Function2(string: String) {
		def / (str: String): Path = Paths.get(string).resolve(str)
		def / (path: Path): Path = Paths.get(string).resolve(path)
	}

	implicit class Function3(path: Path) {
		def write(str: String): Path = Files.write(path, str.getBytes)
	}

	implicit class Function4(path: Path) {
		def read(): String = new String(Files.readAllBytes(path))
	}

	implicit class Function5(path: Path) {
		def append(str: String): Path = {
			if(Files.exists(path)) Files.write(path, str.getBytes, StandardOpenOption.APPEND)
      		else Files.write(path, str.getBytes)
		}
	}
}

object DateImplicits {
	implicit class Function6(day: Int) {
		def jan(): LocalDate  = LocalDate.of(LocalDate.now().getYear, 1, day)
     	def feb(): LocalDate  = LocalDate.of(LocalDate.now().getYear, 2, day)
     	def mar(): LocalDate  = LocalDate.of(LocalDate.now().getYear, 3, day)
     	def apr(): LocalDate  = LocalDate.of(LocalDate.now().getYear, 4, day)
     	def may(): LocalDate  = LocalDate.of(LocalDate.now().getYear, 5, day)
     	def jun(): LocalDate  = LocalDate.of(LocalDate.now().getYear, 6, day)
     	def jul(): LocalDate  = LocalDate.of(LocalDate.now().getYear, 7, day)
     	def aug(): LocalDate  = LocalDate.of(LocalDate.now().getYear, 8, day)
     	def sep(): LocalDate  = LocalDate.of(LocalDate.now().getYear, 9, day)
     	def oct(): LocalDate  = LocalDate.of(LocalDate.now().getYear, 10, day)
     	def nov(): LocalDate  = LocalDate.of(LocalDate.now().getYear, 11, day)
     	def dec(): LocalDate  = LocalDate.of(LocalDate.now().getYear, 12, day)

     	def jan(year: Int): LocalDate  = LocalDate.of(year, 1, day)
     	def feb(year: Int): LocalDate  = LocalDate.of(year, 2, day)
     	def mar(year: Int): LocalDate  = LocalDate.of(year, 3, day)
     	def apr(year: Int): LocalDate  = LocalDate.of(year, 4, day)
     	def may(year: Int): LocalDate  = LocalDate.of(year, 5, day)
     	def jun(year: Int): LocalDate  = LocalDate.of(year, 6, day)
     	def jul(year: Int): LocalDate  = LocalDate.of(year, 7, day)
     	def aug(year: Int): LocalDate  = LocalDate.of(year, 8, day)
     	def sep(year: Int): LocalDate  = LocalDate.of(year, 9, day)
     	def oct(year: Int): LocalDate  = LocalDate.of(year, 10, day)
     	def nov(year: Int): LocalDate  = LocalDate.of(year, 11, day)
     	def dec(year: Int): LocalDate  = LocalDate.of(year, 12, day)
	}

	implicit class Function7(x: Int) {
		def days(): (Int, String) = (x, "days")
    	def months(): (Int, String) = (x, "months")
    	def years(): (Int, String) = (x, "years")
	}

	implicit class Function8(localdate: LocalDate) {
		def + (x: (Int, String)): LocalDate = {
      		if((x._2).equals("days")) localdate.plusDays((x._1))
      		else if((x._2).equals("months")) localdate.plusMonths((x._1))
      		else localdate.plusYears((x._1))
   		}
	}
}