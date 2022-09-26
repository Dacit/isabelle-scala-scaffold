package isabelle.scaffold


import isabelle._


object Scaffold {
  def read_theory(theory_context: Export.Theory_Context): Option[Document.Snapshot] = {
    def read(name: String): Export.Entry =
      theory_context(name, permissive = true)

    def read_xml(name: String): XML.Body =
      YXML.parse_body(
        Symbol.output(unicode_symbols = false, UTF8.decode_permissive(read(name).uncompressed)),
        cache = theory_context.cache)

    for {
      (thy_file, _) <- theory_context.files(permissive = true)
    }
    yield {
      val master_dir =
        Thy_Header.split_file_name(thy_file) match {
          case Some((dir, _)) => dir
          case None => error("Cannot determine theory master directory: " + quote(thy_file))
        }
      val node_name =
        Document.Node.Name(thy_file, master_dir = master_dir, theory = theory_context.theory)

      val thy_xml = read_xml(Export.MARKUP)
      val command_spans = Token_Markup.from_xml(thy_xml)

      val counter = Counter.make()

      val (state0, node_edits, commands, _) = command_spans.foldLeft(
        Document.State.init,
        List.empty[Text.Edit], List.empty[Command], 0) {
        case ((st, ed, cmd, offset), command_span) =>
          val command = Command(counter(), node_name, Command.Blobs_Info.none, command_span)
          (st.define_command(command), ed :+ Text.Edit.insert(offset, command.source),
            cmd :+ command, offset + command.length)
      }

      val version = Document.Version.init
      val nodes0 = version.nodes
      val nodes1 = nodes0 + (node_name -> nodes0(node_name)
        .update_commands(Linear_Set.from(commands)))
      val version1 = Document.Version.make(nodes1)

      val edits: List[Document.Edit_Text] =
        List(node_name -> Document.Node.Edits(node_edits))

      val state1 =
        state0.continue_history(Future.value(version), edits, Future.value(version1))
          .define_version(version1, state0.the_assignment(version))
          .assign(version1.id, Nil, commands.map(c => c.id -> List(Document_ID.make())))._2

      state1.snapshot(node_name = node_name)
    }
  }

  def scaffold(
    options: Options,
    progress: Progress = new Progress,
    dirs: List[Path] = Nil,
    select_dirs: List[Path] = Nil,
    selection: Sessions.Selection = Sessions.Selection.empty,
  ): Unit = {

    /* build theories if not yet done */

    val res =
      Build.build(
        options,
        selection,
        progress = progress,
        dirs = dirs,
        select_dirs = select_dirs)
    if (!res.ok) System.exit(res.rc)

    /* load build snapshot */

    val store = Sessions.store(options)

    val full_sessions =
      Sessions.load_structure(options = options, dirs = dirs, select_dirs = select_dirs)

    val sessions_structure = full_sessions.selection(selection)
    val deps = Sessions.deps(sessions_structure)

    sessions_structure.build_selection(selection).foreach { session_name =>
      val base = deps.get(session_name).getOrElse(error("No base for " + session_name))
      val theories = base.proper_session_theories.map(_.theory)

      using(Export.open_session_context0(store, session_name))(session_context => {
        val result =
          for {
            db <- session_context.session_db()
            theories = store.read_theories(db, session_name)
            errors = store.read_errors(db, session_name)
            info <- store.read_build(db, session_name)
          } yield (theories, errors, info.return_code)
        result match {
          case None => error("Missing build database for session " + quote(session_name))
          case Some((used_theories, errors, _)) =>
            if (errors.nonEmpty) error(errors.mkString("\n\n"))
            used_theories.foreach { thy =>
              val thy_heading = "\nTheory " + quote(thy) + ":"
              read_theory(session_context.theory(thy)) match {
                case None =>
                  progress.echo_warning(thy_heading + " missing")
                  None
                case Some(snapshot) =>
                  // Loads export view + command spans
                  val exports = Export_Theory.read_session(session_context)
                  val command_spans = snapshot.state.commands.values.map(_.command.span)
                  // TODO: do something with the stuff!
              }
            }
        }
      })
    }
  }

  /* Isabelle tool */

  val isabelle_tool =
    Isabelle_Tool("scaffold", "just a scala scaffold", Scala_Project.here, args =>
    {
      var base_sessions: List[String] = Nil
      var select_dirs: List[Path] = Nil
      var requirements = false
      var exclude_session_groups: List[String] = Nil
      var all_sessions = false
      var dirs: List[Path] = Nil
      var session_groups: List[String] = Nil
      var options = Options.init()
      var verbose = false
      var exclude_sessions: List[String] = Nil

      val getopts = Getopts("""
Usage: isabelle scaffold [OPTIONS] [SESSIONS ...]

  Options are:
    -B NAME      include session NAME and all descendants
    -D DIR       include session directory and select its sessions
    -R           refer to requirements of selected sessions
    -X NAME      exclude sessions from group NAME and all descendants
    -a           select all sessions
    -d DIR       include session directory
    -g NAME      select session group NAME
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
    -x NAME      exclude session NAME and all descendants

  Just a scaffold

""",
        "B:" -> (arg => base_sessions = base_sessions ::: List(arg)),
        "D:" -> (arg => select_dirs = select_dirs ::: List(Path.explode(arg))),
        "R" -> (_ => requirements = true),
        "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
        "a" -> (_ => all_sessions = true),
        "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
        "g:" -> (arg => session_groups = session_groups ::: List(arg)),
        "o:" -> (arg => options = options + arg),
        "v" -> (_ => verbose = true),
        "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)))

      /* activate exports */
      options = options + ("export_theory", "true")

      val sessions = getopts(args)

      val progress = new Console_Progress()

      progress.interrupt_handler {
        scaffold(
          options,
          progress = progress,
          dirs = dirs,
          select_dirs = select_dirs,
          selection = Sessions.Selection(
            requirements = requirements,
            all_sessions = all_sessions,
            base_sessions = base_sessions,
            exclude_session_groups = exclude_session_groups,
            exclude_sessions = exclude_sessions,
            session_groups = session_groups,
            sessions = sessions
          ))
      }
    })
}

class Tools extends Isabelle_Scala_Tools(Scaffold.isabelle_tool)
