defmodule ListServerTest  do
  use ExUnit.Case

  setup do
    ListServer.start_link :test
    ListServer.clear :test
  end

  test "it starts out empty" do
    assert ListServer.items(:test) == []
  end

  test "it lets us add things to the list" do
    ListServer.add(:test, "book")
    assert ListServer.items(:test) == ["book"]
  end

  test "it lets us remove things from the list" do
    ListServer.add(:test, "book")
    ListServer.add(:test, "magazine")
    ListServer.remove(:test, "book")
    assert ListServer.items(:test) == ["magazine"]
  end

  test "it restart process empty" do

  end

end
