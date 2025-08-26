defmodule DmarcWeb.RecipientLive.Form do
  use DmarcWeb, :live_view

  alias Dmarc.Recipients
  alias Dmarc.Recipients.Recipient

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <.header>
        {@page_title}
        <:subtitle>Use this form to manage recipient records in your database.</:subtitle>
      </.header>

      <.form for={@form} id="recipient-form" phx-change="validate" phx-submit="save">
        <.input field={@form[:email]} type="text" label="Email" />
        <footer>
          <.button phx-disable-with="Saving..." variant="primary">Save Recipient</.button>
          <.button navigate={return_path(@current_scope, @return_to, @recipient)}>Cancel</.button>
        </footer>
      </.form>
    </Layouts.app>
    """
  end

  @impl true
  def mount(params, _session, socket) do
    {:ok,
     socket
     |> assign(:return_to, return_to(params["return_to"]))
     |> apply_action(socket.assigns.live_action, params)}
  end

  defp return_to("show"), do: "show"
  defp return_to(_), do: "index"

  defp apply_action(socket, :edit, %{"id" => id}) do
    recipient = Recipients.get_recipient!(socket.assigns.current_scope, id)

    socket
    |> assign(:page_title, "Edit Recipient")
    |> assign(:recipient, recipient)
    |> assign(:form, to_form(Recipients.change_recipient(socket.assigns.current_scope, recipient)))
  end

  defp apply_action(socket, :new, _params) do
    recipient = %Recipient{customer_id: socket.assigns.current_scope.customer.id}

    socket
    |> assign(:page_title, "New Recipient")
    |> assign(:recipient, recipient)
    |> assign(:form, to_form(Recipients.change_recipient(socket.assigns.current_scope, recipient)))
  end

  @impl true
  def handle_event("validate", %{"recipient" => recipient_params}, socket) do
    changeset = Recipients.change_recipient(socket.assigns.current_scope, socket.assigns.recipient, recipient_params)
    {:noreply, assign(socket, form: to_form(changeset, action: :validate))}
  end

  def handle_event("save", %{"recipient" => recipient_params}, socket) do
    save_recipient(socket, socket.assigns.live_action, recipient_params)
  end

  defp save_recipient(socket, :edit, recipient_params) do
    case Recipients.update_recipient(socket.assigns.current_scope, socket.assigns.recipient, recipient_params) do
      {:ok, recipient} ->
        {:noreply,
         socket
         |> put_flash(:info, "Recipient updated successfully")
         |> push_navigate(
           to: return_path(socket.assigns.current_scope, socket.assigns.return_to, recipient)
         )}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, form: to_form(changeset))}
    end
  end

  defp save_recipient(socket, :new, recipient_params) do
    case Recipients.create_recipient(socket.assigns.current_scope, recipient_params) do
      {:ok, recipient} ->
        {:noreply,
         socket
         |> put_flash(:info, "Recipient created successfully")
         |> push_navigate(
           to: return_path(socket.assigns.current_scope, socket.assigns.return_to, recipient)
         )}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, form: to_form(changeset))}
    end
  end

  defp return_path(_scope, "index", _recipient), do: ~p"/recipients"
  defp return_path(_scope, "show", recipient), do: ~p"/recipients/#{recipient}"
end
