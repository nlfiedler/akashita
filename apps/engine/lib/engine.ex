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
end
