defmodule AkashitaEngine do
  @moduledoc """

  The AES/CBC and AES/CTR encryption algorithms require that the initialization
  vector (IV) is 16 bytes, while the key may be 16, 24, or 32 bytes.

  """

  @aes_block_size 16

  @doc """

  Pad the binary data to a multiple of the given size.

  """
  def pad(data, block_size) do
    to_add = block_size - rem(byte_size(data), block_size)
    data <> to_string(:string.chars(to_add, to_add))
  end

  @doc """

  Reverse of pad/2 which removes the padding from the data.

  """
  def unpad(data) do
    to_remove = :binary.last(data)
    :binary.part(data, 0, byte_size(data) - to_remove)
  end

  @doc """

  Encrypt the data with a key and initialization vector using AES/CBC.

  """
  def encrypt(plaintext, key, iv) do
    :crypto.block_encrypt(:aes_cbc, key, iv, pad(plaintext, @aes_block_size))
  end

  @doc """

  Decrypt the data with a key and initialization vector using AES/CBC.

  """
  def decrypt(ciphertext, key, iv) do
    unpad(:crypto.block_decrypt(:aes_cbc, key, iv, ciphertext))
  end

  @doc """

  Encrypt the file, writing the results to the desired path, using AES/CTR.

  Returns :ok or {:error, reason}

  """
  def encrypt_file(infile, outfile, key, iv) do
    state = :crypto.stream_init(:aes_ctr, key, iv)
    fun = &:crypto.stream_encrypt(&1, &2)
    with {:ok, id} = File.open(infile, [:read, :binary, :read_ahead]),
         {:ok, od} = File.open(outfile, [:write, :delayed_write]),
         do: crypt_device(id, od, state, fun)
  end

  @doc """

  Decrypt the file, writing the results to the desired path, using AES/CTR.

  Returns :ok or {:error, reason}

  """
  def decrypt_file(infile, outfile, key, iv) do
    state = :crypto.stream_init(:aes_ctr, key, iv)
    fun = &:crypto.stream_decrypt(&1, &2)
    with {:ok, id} = File.open(infile, [:read, :binary, :read_ahead]),
         {:ok, od} = File.open(outfile, [:write, :delayed_write]),
         do: crypt_device(id, od, state, fun)
  end

  # Read from the input device in chunks, passing the data through the stream
  # cipher, and writing the results to the output device. The given function
  # should be either :crypto.stream_encrypt/2 or :crypto.stream_decrypt/2.
  defp crypt_device(indevice, outdevice, state, fun) do
    case IO.binread(indevice, 65536) do
      :eof ->
        case File.close(indevice) do
          :ok -> File.close(outdevice)
          err -> err
        end
      {:error, reason} ->
        {:error, reason}
      data ->
        {new_state, transformed} = fun.(state, data)
        IO.binwrite(outdevice, transformed)
        crypt_device(indevice, outdevice, new_state, fun)
    end
  end

  @doc """

  Generate the two 32 byte master keys, return {:ok, master1, master2}.

  """
  def generate_master_keys() do
    master1 = :crypto.strong_rand_bytes(32)
    master2 = :crypto.strong_rand_bytes(32)
    {:ok, master1, master2}
  end

  @doc """

  Use the user password and the two master keys to produce the encryption
  data that will be stored in the database.

  Returns {:ok, salt, init_vector, HMAC, encrypted_keys}

  """
  def new_master_encryption_data(password, master1, master2) do
    salt = :crypto.strong_rand_bytes(16)
    iv = :crypto.strong_rand_bytes(16)
    key = hash_password(password, salt)
    encrypted = encrypt(master1 <> master2, key, iv)
    hmac = :crypto.hmac(:sha256, key, iv <> encrypted)
    {:ok, salt, iv, hmac, encrypted}
  end

  @doc """

  Decrypt the master keys with the given data.

  {:ok, master1, master2}

  """
  def decrypt_master_keys(salt, password, iv, encrypted, hmac) do
    key = hash_password(password, salt)
    hmac2 = :crypto.hmac(:sha256, key, iv <> encrypted)
    if hmac != hmac2 do
      raise "HMAC does not match records"
    end
    plaintext = decrypt(encrypted, key, iv)
    <<master1::binary-size(32), master2::binary-size(32)>> = plaintext
    {:ok, master1, master2}
  end

  @doc """

  Hash the given password using the salt and PBKDF2 algorithm, with 160,000
  rounds, using the SHA256 hash, and returning a 32 byte value.

  """
  def hash_password(password, salt) do
    pbkdf2(password, salt, 160000, :sha256, 32, 1, [], 0)
  end

  # Basic implementation of the PBKDF2 algorithm for hashing passwords.
  defp pbkdf2(_password, _salt, _rounds, _digest, max_len, _block_index, acc, length)
      when length >= max_len do
    key = acc |> Enum.reverse |> IO.iodata_to_binary
    <<bin::binary-size(max_len), _::binary>> = key
    bin
  end
  defp pbkdf2(password, salt, rounds, digest, max_len, block_index, acc, length) do
    initial = :crypto.hmac(digest, password, <<salt::binary, block_index::integer-size(32)>>)
    block = pbkdf2_round(password, rounds - 1, digest, initial, initial)
    pbkdf2(password, salt, rounds, digest, max_len, block_index + 1,
           [block | acc], byte_size(block) + length)
  end

  defp pbkdf2_round(_password, 0, _digest, _prev, acc), do: acc
  defp pbkdf2_round(password, round, digest, prev, acc) do
    next = :crypto.hmac(digest, password, prev)
    pbkdf2_round(password, round - 1, digest, next, :crypto.exor(next, acc))
  end

  @doc """

  Generate a type 5 UUID for the computer, using the current user's home
  directory and the host name.

  """
  def gen_computer_uuid() do
    userhome = case System.user_home() do
      nil -> "/unknown"
      home -> home
    end
    {:ok, host_chars} = :inet.gethostname()
    name = to_string(host_chars) <> ":" <> userhome
    :inugami.encode(:inugami.uuid5(:inugami.namespace_url(), name))
  end

  @doc """

  Generate a suitable bucket name, using a ULID and the computer UUID.

  """
  def gen_bucket_name() do
    uuid = String.replace(gen_computer_uuid(), "-", "")
    ulid = String.downcase(Ulid.generate())
    ulid <> uuid
  end

  @doc """

  Create a pack file consisting of the given file (parts). The input is a list
  of 3-tuples: {file path, byte offset, byte length}. Returns {:ok, sha256,
  map of SHA1 to byte offsets}.

  """
  def pack_files(files, outfile)
      when is_list(files)
       and length(files) > 1
       and is_binary(outfile)
       and byte_size(outfile) > 1 do
    pack_context = :crypto.hash_init(:sha256)
    {:ok, pack_dev} = File.open(outfile, [:write, :delayed_write])
    pack_context = write_and_hash("P4CK", pack_dev, pack_context)
    pack_context = write_and_hash(<<0, 0, 0, 1>>, pack_dev, pack_context)
    file_count = length(files)
    pack_context = write_and_hash(<<file_count::32>>, pack_dev, pack_context)
    {_pdev, _poff, pack_context, hash_to_offset_map} = Enum.reduce(
      files, {pack_dev, 0, pack_context, Map.new()}, &pack_file_enum/2)
    pack_digest = :crypto.hash_final(pack_context)
    :ok = File.close(pack_dev)
    {:ok, pack_digest, hash_to_offset_map}
  end

  #
  # Write the given data to the output device and the hasher, returning the
  # updated hash context.
  #
  defp write_and_hash(data, dev, context) do
    IO.binwrite(dev, data)
    :crypto.hash_update(context, data)
  end

  #
  # Invoked once for each 3-tuple in the list of files passed to pack_files/2.
  # The accumulator consists of the pack file (output) device, the pack file
  # digest context, and the map of file part SHA1 digest to byte offset within
  # the pack file.
  #
  defp pack_file_enum({path, pos, len}, {dev, offset, context, entry_map}) do
    len_bytes = <<len::32>>
    context = write_and_hash(len_bytes, dev, context)
    {:ok, file_dev} = File.open(path, [:read, :binary, :read_ahead])
    file_context = :crypto.hash_init(:sha)
    {:ok, _np} = :file.position(file_dev, pos)
    {:ok, context, file_hash} = pack_file(context, file_context, dev, file_dev, len)
    entry_map = Map.put(entry_map, file_hash, offset)
    # Return the offset of the next part, which is the length of this part plus
    # 4 bytes for the length value itself.
    {dev, offset + len + 4, context, entry_map}
  end

  #
  # Copy length bytes from file_dev to pack_dev, returning {:ok, sha1}.
  #
  defp pack_file(pack_context, file_context, _pack_dev, file_dev, 0) do
    :ok = File.close(file_dev)
    {:ok, pack_context, :crypto.hash_final(file_context)}
  end
  defp pack_file(pack_context, file_context, pack_dev, file_dev, length) do
    to_read = min(length, 65536)
    case IO.binread(file_dev, to_read) do
      :eof -> pack_file(pack_context, file_context, pack_dev, file_dev, 0)
      {:error, reason} ->
        {:error, reason}
      data ->
        pack_context = write_and_hash(data, pack_dev, pack_context)
        file_context = :crypto.hash_update(file_context, data)
        pack_file(pack_context, file_context, pack_dev, file_dev, length - to_read)
    end
  end

  @doc """

  Compute the digest of the given file, returning {:ok, digest}. The default
  digest is :sha256, but any value acceptable to :crypto.hash_init/1 is
  supported.

  """
  def compute_digest(filename, digest \\ :sha256) do
    {:ok, handle} = File.open(filename, [:read, :binary, :read_ahead])
    context = :crypto.hash_init(digest)
    case compute_digest_iter(handle, context) do
      {:ok, digest} -> {:ok, Base.encode16(digest, case: :lower)}
      err -> err
    end
  end

  #
  # Recursively compute the digest of the bytes read from the input device,
  # return {:ok, digest_bytes}.
  #
  defp compute_digest_iter(handle, context) do
    case IO.binread(handle, 65536) do
      :eof ->
        :ok = File.close(handle)
        {:ok, :crypto.hash_final(context)}
      {:error, reason} ->
        {:error, reason}
      data ->
        context = :crypto.hash_update(context, data)
        compute_digest_iter(handle, context)
    end
  end

  @doc """

  Extract the file parts from the given pack file, writing them to the
  output directory, with the names being the SHA1 checksum of each part.
  Returns :ok or {:error, reason}.

  """
  def unpack_files(pack_file, outdir) do
    {:ok, handle} = File.open(pack_file, [:read, :binary, :read_ahead])
    File.mkdir_p!(outdir)
    # TODO: handle an encrypted pack file
    case IO.binread(handle, 12) do
      :eof -> {:error, "missing pack header"}
      {:error, reason} -> {:error, reason}
      <<"P4CK", 0,0,0,1, count :: size(32)>> ->
        unpack_files(count, handle, outdir)
      _unknown -> {:error, "unsupported pack file"}
    end
  end

  #
  # Unpack the parts from the device.
  #
  defp unpack_files(0, phandle, _outdir) do
    File.close(phandle)
  end
  defp unpack_files(count, phandle, outdir) do
    case IO.binread(phandle, 4) do
      :eof -> {:error, "missing part size"}
      {:error, reason} -> {:error, reason}
      data ->
        to_read = :binary.decode_unsigned(data)
        context = :crypto.hash_init(:sha)
        outfile = Path.join(outdir, "p4ck-p4rt-#{to_read}")
        {:ok, fhandle} = File.open(outfile, [:write, :delayed_write])
        case unpack_file(phandle, context, fhandle, to_read) do
          {:ok, digest} ->
            sha1file = Path.join(outdir, Base.encode16(digest, case: :lower))
            :ok = File.rename(outfile, sha1file)
            unpack_files(count - 1, phandle, outdir)
          err -> err
        end
    end
  end

  #
  # Unpack a single part from the pack file.
  #
  defp unpack_file(_phandle, context, fhandle, 0) do
    :ok = File.close(fhandle)
    {:ok, :crypto.hash_final(context)}
  end
  defp unpack_file(phandle, context, fhandle, length) do
    to_read = min(length, 65536)
    case IO.binread(phandle, to_read) do
      :eof -> unpack_file(phandle, context, fhandle, 0)
      {:error, reason} -> {:error, reason}
      data ->
        context = write_and_hash(data, fhandle, context)
        unpack_file(phandle, context, fhandle, length - to_read)
    end
  end
end
