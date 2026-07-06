package home.textboard

case class Text(str: String)

object Text:
  def mkText(str: String): Option[Text] =
    Option.when(str.length < 200)(Text(str))

object Read:
  case class Post(id: Int, text: Text)
  case class PostDTO(id: Int, str: String)

object Write:
  case class Post(text: Text)
  case class PostDTO(str: String)

  def fromDTO(post: PostDTO): Option[Post] =
    Text.mkText(post.str).map(Post.apply)
