import java.awt.datatransfer.StringSelection

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.ide.CopyPasteManager

trait TemplateGenerator extends AnAction {

  def generateTemplate(text: String): String

  override def actionPerformed(e: AnActionEvent): Unit = {
    val editor         = e.getRequiredData(CommonDataKeys.EDITOR)
    val selectionModel = editor.getSelectionModel
    Option(selectionModel.getSelectedText) match {
      case Some(text) if !text.isEmpty =>
        val template = generateTemplate(text)
        CopyPasteManager.getInstance().setContents(new StringSelection(template))
      case _ => ()
    }
  }

  override def update(e: AnActionEvent): Unit = {
    val project = e.getProject
    val editor  = e.getData(CommonDataKeys.EDITOR)

    e.getPresentation.setVisible(
      project != null
      && editor != null
      && editor.getSelectionModel.hasSelection
    )
  }

}
