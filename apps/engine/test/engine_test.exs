defmodule AkashitaEngineTest do
  use ExUnit.Case

  setup_all do
    # create a temporary workspace for the tests
    priv_dir = Temp.mkdir!("akashita")
    on_exit(:tmp_dir_rm, fn() -> File.rm_rf(priv_dir) end)
    {:ok, priv_dir: priv_dir}
  end

  test "encrypt/decrypt with AES/CBC" do
    key = :crypto.strong_rand_bytes(32)
    iv = :crypto.strong_rand_bytes(16)
    # data is a multiple of padding size
    in_data = :crypto.strong_rand_bytes(128)
    cipher = AkashitaEngine.encrypt(in_data, key, iv)
    assert byte_size(cipher) == 144
    out_data = AkashitaEngine.decrypt(cipher, key, iv)
    assert byte_size(out_data) == 128
    assert in_data == out_data

    # data is not a multiple of padding size
    in_data = :crypto.strong_rand_bytes(111)
    cipher = AkashitaEngine.encrypt(in_data, key, iv)
    assert byte_size(cipher) == 112
    out_data = AkashitaEngine.decrypt(cipher, key, iv)
    assert byte_size(out_data) == 111
    assert in_data == out_data
  end

  test "encrypt/decrypt file with AES/CTR", context do
    priv_dir = context[:priv_dir]
    infile = "./test/fixtures/SekienAkashita.jpg"
    outfile = Path.join(priv_dir, "SekienAkashita.aes")
    key = :crypto.strong_rand_bytes(32)
    iv = :crypto.strong_rand_bytes(16)
    :ok = AkashitaEngine.encrypt_file(infile, outfile, key, iv)
    tmpfile = Path.join(priv_dir, "SekienAkashita.jpg")
    :ok = AkashitaEngine.decrypt_file(outfile, tmpfile, key, iv)
    original = File.read!(infile)
    roundtrip = File.read!(tmpfile)
    assert original == roundtrip
  end

  test "generate encryption data" do
    password = "my password is secret"
    {:ok, master1, master2} = AkashitaEngine.generate_master_keys()
    {:ok, salt, iv, hmac, encrypted} =
      AkashitaEngine.new_master_encryption_data(password, master1, master2)
    {:ok, blaster1, blaster2} = AkashitaEngine.decrypt_master_keys(
      salt, password, iv, encrypted, hmac)
    assert master1 == blaster1
    assert master2 == blaster2
  end

  test "computer UUID" do
    uuid = AkashitaEngine.gen_computer_uuid()
    assert is_binary(uuid)
    assert byte_size(uuid) == 36
    assert :inugami.get_version(:inugami.decode(uuid)) == 5
  end

  test "bucket name" do
    bn = AkashitaEngine.gen_bucket_name()
    assert is_binary(bn)
    assert byte_size(bn) == 58
  end
end
