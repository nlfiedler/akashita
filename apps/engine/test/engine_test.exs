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

  test "compute SHA256" do
    {:ok, actual} = AkashitaEngine.compute_digest("./test/fixtures/NOTICE")
    assert actual == "907b089766f2a951214c24b0f9a616fc0ccfc4d9e1b7acd1c22e916ad98c1434"
    {:ok, actual} = AkashitaEngine.compute_digest("./test/fixtures/SekienAkashita.jpg")
    assert actual == "d9e749d9367fc908876749d6502eb212fee88c9a94892fb07da5ef3ba8bc39ed"
  end

  test "pack files bad input" do
    assert_raise FunctionClauseError, fn ->
      AkashitaEngine.pack_files([], "")
    end
    assert_raise FunctionClauseError, fn ->
      AkashitaEngine.pack_files([1], "")
    end
    assert_raise FunctionClauseError, fn ->
      AkashitaEngine.pack_files([1], 1)
    end
    assert_raise FunctionClauseError, fn ->
      AkashitaEngine.pack_files([1], "a")
    end
  end

  test "pack and unpack files", context do
    priv_dir = context[:priv_dir]
    #
    # Pack up some test files.
    #
    files = [
      {"./test/fixtures/NOTICE", 0, 131},
      {"./test/fixtures/SekienAkashita.jpg", 0, 512},
      {"./test/fixtures/SekienAkashita.jpg", 0, 109466}
    ]
    tmp_pack = Path.join(priv_dir, "mypackfile")
    {:ok, pack_digest, mapping} = AkashitaEngine.pack_files(files, tmp_pack)
    pack_digest_a = Base.encode16(pack_digest, case: :lower)
    {:ok, digest_bin} = Base.decode16("8D6D4067D6F0A8CC095FE9E3C1311A8339B204EC")
    assert Map.has_key?(mapping, digest_bin)
    assert Map.get(mapping, digest_bin) == 0
    {:ok, digest_bin} = Base.decode16("E17861E7A6D7B64660980A3D7C8484A2500D3125")
    assert Map.has_key?(mapping, digest_bin)
    assert Map.get(mapping, digest_bin) == 135
    {:ok, digest_bin} = Base.decode16("4C009E44FE5794DF0B1F828F2A8C868E66644964")
    assert Map.has_key?(mapping, digest_bin)
    assert Map.get(mapping, digest_bin) == 651
    {:ok, pack_digest_e} = AkashitaEngine.compute_digest(tmp_pack)
    assert pack_digest_e == "ebb51b43aeb6880bfeabc367d2aa7b0e070a16d6fdda628292ffdae7cac1af4d"
    assert pack_digest_a == pack_digest_e
    #
    # Unpack the files and validate the results match what went in, including
    # checksum and byte count.
    #
    :ok = AkashitaEngine.unpack_files(tmp_pack, priv_dir)
    expected_parts = [
      {"8d6d4067d6f0a8cc095fe9e3c1311a8339b204ec", 131},
      {"e17861e7a6d7b64660980a3d7c8484a2500d3125", 512},
      {"4c009e44fe5794df0b1f828f2a8c868e66644964", 109466}
    ]
    for {sha1, length} <- expected_parts do
      fname = Path.join(priv_dir, sha1)
      assert File.exists?(fname)
      stat = File.stat!(fname)
      assert stat.size == length
      {:ok, digest} = AkashitaEngine.compute_digest(fname, :sha)
      assert digest == sha1
    end
  end

  test "pack and unpack files encrypted", context do
    priv_dir = context[:priv_dir]
    {:ok, master1, master2} = AkashitaEngine.generate_master_keys()
    #
    # Pack up some test files.
    #
    files = [
      {"./test/fixtures/NOTICE", 0, 131},
      {"./test/fixtures/SekienAkashita.jpg", 0, 512},
      {"./test/fixtures/SekienAkashita.jpg", 0, 109466}
    ]
    tmp_pack = Path.join(priv_dir, "mypackfile")
    # The pack digest is useful information, but we cannot verify on the
    # encrypted pack file since the digest is of the pack _before_ encryption.
    {:ok, _pack_digest, mapping} = AkashitaEngine.pack_files_encrypted(
      files, tmp_pack, master1, master2)
    {:ok, digest_bin} = Base.decode16("8D6D4067D6F0A8CC095FE9E3C1311A8339B204EC")
    assert Map.has_key?(mapping, digest_bin)
    assert Map.get(mapping, digest_bin) == 0
    {:ok, digest_bin} = Base.decode16("E17861E7A6D7B64660980A3D7C8484A2500D3125")
    assert Map.has_key?(mapping, digest_bin)
    assert Map.get(mapping, digest_bin) == 135
    {:ok, digest_bin} = Base.decode16("4C009E44FE5794DF0B1F828F2A8C868E66644964")
    assert Map.has_key?(mapping, digest_bin)
    assert Map.get(mapping, digest_bin) == 651
    #
    # Unpack the files and validate the results match what went in, including
    # checksum and byte count.
    #
    :ok = AkashitaEngine.unpack_files_encrypted(
      tmp_pack, priv_dir, master1, master2)
    expected_parts = [
      {"8d6d4067d6f0a8cc095fe9e3c1311a8339b204ec", 131},
      {"e17861e7a6d7b64660980a3d7c8484a2500d3125", 512},
      {"4c009e44fe5794df0b1f828f2a8c868e66644964", 109466}
    ]
    for {sha1, length} <- expected_parts do
      fname = Path.join(priv_dir, sha1)
      assert File.exists?(fname)
      stat = File.stat!(fname)
      assert stat.size == length
      {:ok, digest} = AkashitaEngine.compute_digest(fname, :sha)
      assert digest == sha1
    end
  end
end
