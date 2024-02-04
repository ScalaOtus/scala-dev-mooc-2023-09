package module1.datacollections.DataCollection2

//Вариантность (Variances) - это указание определенной специфики взаимосвязи между связанными типами. Scala поддерживает
// вариантную аннотацию типов у обобщенных классов, что позволяет им быть ковариантными, контрвариантными или
// инвариантными (если нет никакого указания на вариантность). Использование вариантности в системе типов позволяет
// устанавливать понятные взаимосвязи между сложными типами, в то время как отсутствие вариантности может ограничить
// повторное использование абстракции класса.
object Variation {
  abstract class Animal {
    def name: String
  }
  case class Cat(name: String) extends Animal
  case class Dog(name: String) extends Animal
  class Box[A](var content: A)

  def main(args: Array[String]): Unit ={
    val myAnimal: Animal = Cat("Felix")

    //Инвариантность
    val myCatBox: Box[Cat] = new Box[Cat](Cat("Felix"))
    //Является ли Box[Cat] подтипом Box[Animal], как Cat подтип Animal?
    // На первый взгляд может показаться, что это правдоподобно, но если мы
    // попытаемся это сделать, компилятор сообщит об ошибке:
    // val myAnimalBox: Box[Animal] = myCatBox // не компилируется
    //    val myAnimal: Animal = myAnimalBox.content

    //Почему это может быть проблемой? Мы можем достать из контейнера кота, и это все еще животное, не так ли?
    // Ну да. Но это не все, что мы можем сделать. Мы также можем заменить в контейнере кота другим животным.
    // myAnimalBox.content = Dog("Fido")
    //Теперь в контейнере для животных есть собака. Все в порядке, вы можете поместить собак в
    // контейнеры для животных, потому что собаки — это животные. Но наш контейнер для животных — это
    // контейнер для котов! Нельзя поместить собаку в контейнер с котом. Если бы мы могли, а затем
    // попытались достать кота из нашего кошачьего контейнера, он оказался бы собакой, нарушающей целостность типа.
    //val myCat: Cat = myCatBox.content // myCat стал бы собакой Fido!

    //Из этого мы должны сделать вывод, что между Box[Cat] и
    // Box[Animal] не может быть отношения подтипа, хотя между Cat и Animal это отношение есть.

    //Ковариантность
    class ImmutableBox[+A](val content: A)
    val catbox: ImmutableBox[Cat] = new ImmutableBox[Cat](Cat("Felix"))
    val animalBox: ImmutableBox[Animal] = catbox // теперь код компилируется
    //Мы говорим, что ImmutableBox ковариантен в A - на это указывает + перед A.
    //
    //Более формально это дает нам следующее отношение: если
    // задано некоторое class Cov[+T], то если A является подтипом B,
    // то Cov[A] является подтипом Cov[B]. Это позволяет создавать очень
    // полезные и интуитивно понятные отношения подтипов с помощью обобщения.

    //Контрвариантность
    //что-то, что можно положить, но нельзя вынуть? Такая ситуация возникает, если у
    // нас есть что-то вроде сериализатора, который принимает значения типа A и
    // преобразует их в сериализованный формат.
    //Мы говорим, что Serializer контравариантен в A, и на это указывает - перед A. Более общий сериализатор является подтипом более конкретного сериализатора.
    //
    //Более формально это дает нам обратное отношение: если задано некоторое
    // class Contra[-T], то если A является подтипом B, Contra[B] является подтипом Contra[A].
    abstract class Serializer[-A] {
      def serialize(a: A): String
    }

    val animalSerializer: Serializer[Animal] = new Serializer[Animal] {
      def serialize(animal: Animal): String = s"""{ "name": "${animal.name}" }"""
    }
    val catSerializer: Serializer[Cat] = animalSerializer
    catSerializer.serialize(Cat("Felix"))
  }

}