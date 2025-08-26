defmodule DmarcWeb.RecipientLive.Show do
  use DmarcWeb, :live_view

  alias Dmarc.Recipients

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <.header>
        Recipient {@recipient.id}
        <:subtitle>This is a recipient record from your database.</:subtitle>
        <:actions>
          <.button navigate={~p"/recipients"}>
            <.icon name="hero-arrow-left" />
          </.button>
          <.button variant="primary" navigate={~p"/recipients/#{@recipient}/edit?return_to=show"}>
            <.icon name="hero-pencil-square" /> Edit recipient
          </.button>
        </:actions>
      </.header>

      <.list>
        <:item title="Email">{@recipient.email}</:item>
      </.list>
    </Layouts.app>
    """
  end

  @impl true
  def mount(%{"id" => id}, _session, socket) do
    if connected?(socket) do
      Recipients.subscribe_recipients(socket.assigns.current_scope)
    end

    {:ok,
     socket
     |> assign(:page_title, "Show Recipient")
     |> assign(:recipient, Recipients.get_recipient!(socket.assigns.current_scope, id))}
  end

  @impl true
  def handle_info(
        {:updated, %Dmarc.Recipients.Recipient{id: id} = recipient},
        %{assigns: %{recipient: %{id: id}}} = socket
      ) do
    {:noreply, assign(socket, :recipient, recipient)}
  end

  def handle_info(
        {:deleted, %Dmarc.Recipients.Recipient{id: id}},
        %{assigns: %{recipient: %{id: id}}} = socket
      ) do
    {:noreply,
     socket
     |> put_flash(:error, "The current recipient was deleted.")
     |> push_navigate(to: ~p"/recipients")}
  end

  def handle_info({type, %Dmarc.Recipients.Recipient{}}, socket)
      when type in [:created, :updated, :deleted] do
    {:noreply, socket}
  end
end
