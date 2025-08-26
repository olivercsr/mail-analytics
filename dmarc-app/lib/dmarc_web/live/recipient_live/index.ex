defmodule DmarcWeb.RecipientLive.Index do
  use DmarcWeb, :live_view

  alias Dmarc.Recipients

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <.header>
        Listing Recipients
        <:actions>
          <.button variant="primary" navigate={~p"/recipients/new"}>
            <.icon name="hero-plus" /> New Recipient
          </.button>
        </:actions>
      </.header>

      <.table
        id="recipients"
        rows={@streams.recipients}
        row_click={fn {_id, recipient} -> JS.navigate(~p"/recipients/#{recipient}") end}
      >
        <:col :let={{_id, recipient}} label="Email">{recipient.email}</:col>
        <:action :let={{_id, recipient}}>
          <div class="sr-only">
            <.link navigate={~p"/recipients/#{recipient}"}>Show</.link>
          </div>
          <.link navigate={~p"/recipients/#{recipient}/edit"}>Edit</.link>
        </:action>
        <:action :let={{id, recipient}}>
          <.link
            phx-click={JS.push("delete", value: %{id: recipient.id}) |> hide("##{id}")}
            data-confirm="Are you sure?"
          >
            Delete
          </.link>
        </:action>
      </.table>
    </Layouts.app>
    """
  end

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Recipients.subscribe_recipients(socket.assigns.current_scope)
    end

    {:ok,
     socket
     |> assign(:page_title, "Listing Recipients")
     |> stream(:recipients, Recipients.list_recipients(socket.assigns.current_scope))}
  end

  @impl true
  def handle_event("delete", %{"id" => id}, socket) do
    recipient = Recipients.get_recipient!(socket.assigns.current_scope, id)
    {:ok, _} = Recipients.delete_recipient(socket.assigns.current_scope, recipient)

    {:noreply, stream_delete(socket, :recipients, recipient)}
  end

  @impl true
  def handle_info({type, %Dmarc.Recipients.Recipient{}}, socket)
      when type in [:created, :updated, :deleted] do
    {:noreply, stream(socket, :recipients, Recipients.list_recipients(socket.assigns.current_scope), reset: true)}
  end
end
