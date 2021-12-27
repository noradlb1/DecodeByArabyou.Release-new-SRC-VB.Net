Imports System
Imports System.Collections.Generic
Imports System.ComponentModel
Imports System.Diagnostics
Imports System.Drawing
Imports System.IO
Imports System.IO.Compression
Imports System.Runtime.CompilerServices
Imports System.Security.Cryptography
Imports System.Text
Imports System.Text.RegularExpressions
Imports System.Threading
Imports System.Windows.Forms
Imports Microsoft.VisualBasic
Imports Microsoft.VisualBasic.CompilerServices

Public Class Form1
    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Interaction.MsgBox("卍♕ƧŪĦĀĪ乃 Ō₣ JŌṜƉĀ₦♕卍", MsgBoxStyle.ApplicationModal, Nothing)
        Interaction.MsgBox("卍♕ὋṂᾋȒ ᾋĿ ἿȒᾋQ♕卍", MsgBoxStyle.ApplicationModal, Nothing)
    End Sub

    Public Function DecBin(ByVal str As String) As String
        Dim str2 As String = Regex.Replace(str, "[^01]", "")
        Dim bytes As Byte() = New Byte((CInt(Math.Round(CDbl(((CDbl(str2.Length) / 8) - 1)))) + 1) - 1) {}
        Dim num2 As Integer = (bytes.Length - 1)
        Dim i As Integer = 0
        Do While (i <= num2)
            bytes(i) = Convert.ToByte(str2.Substring((i * 8), 8), 2)
            i += 1
        Loop
        Return Encoding.ASCII.GetString(bytes)
    End Function
    Public Function md5Decrypt(ByVal bytData As Byte(), ByVal sKey As String, Optional ByVal tMode As CipherMode = 2, Optional ByVal tPadding As PaddingMode = 2) As Byte()
        Dim provider As New MD5CryptoServiceProvider
        Dim buffer As Byte() = provider.ComputeHash(Encoding.UTF8.GetBytes(sKey))
        provider.Clear()
        Dim provider2 As New TripleDESCryptoServiceProvider With { _
            .Key = buffer, _
            .Mode = tMode, _
            .Padding = tPadding _
        }
        Dim buffer2 As Byte() = provider2.CreateDecryptor.TransformFinalBlock(bytData, 0, bytData.Length)
        provider2.Clear()
        Return buffer2
    End Function

    Public Function AES_Decrypt(ByVal input As String, ByVal pass As String) As String
        Dim str As String
        Dim managed As New RijndaelManaged
        Dim provider As New MD5CryptoServiceProvider
        Try
            Dim destinationArray As Byte() = New Byte(&H20 - 1) {}
            Dim sourceArray As Byte() = provider.ComputeHash(Encoding.ASCII.GetBytes(pass))
            Array.Copy(sourceArray, 0, destinationArray, 0, &H10)
            Array.Copy(sourceArray, 0, destinationArray, 15, &H10)
            managed.Key = destinationArray
            managed.Mode = CipherMode.ECB
            Dim transform As ICryptoTransform = managed.CreateDecryptor
            Dim inputBuffer As Byte() = Convert.FromBase64String(input)
            str = Encoding.ASCII.GetString(transform.TransformFinalBlock(inputBuffer, 0, inputBuffer.Length))
        Catch exception1 As Exception
            ProjectData.SetProjectError(exception1)
            Dim exception As Exception = exception1
            ProjectData.ClearProjectError()
        End Try
        Return str
    End Function

    Public Function DecompressGZip(ByVal bytesToDecompress As Byte()) As Byte()
        Using stream As GZipStream = New GZipStream(New MemoryStream(bytesToDecompress), CompressionMode.Decompress)
            Dim array As Byte() = New Byte(&H1000 - 1) {}
            Using stream2 As MemoryStream = New MemoryStream
                Dim num As Integer
                Do
                    num = stream.Read(array, 0, &H1000)
                    If (num > 0) Then
                        stream2.Write(array, 0, num)
                    End If
                Loop While (num > 0)
                Return stream2.ToArray
            End Using
        End Using
    End Function
    Public Function DES_Decrypt(ByVal input As String, ByVal pass As String) As String
        Dim str2 As String
        Dim provider As New DESCryptoServiceProvider
        Dim provider2 As New MD5CryptoServiceProvider
        Try
            Dim destinationArray As Byte() = New Byte(8 - 1) {}
            Array.Copy(provider2.ComputeHash(Encoding.ASCII.GetBytes(pass)), 0, destinationArray, 0, 8)
            provider.Key = destinationArray
            provider.Mode = CipherMode.ECB
            Dim transform As ICryptoTransform = provider.CreateDecryptor
            Dim inputBuffer As Byte() = Convert.FromBase64String(input)
            str2 = Encoding.ASCII.GetString(transform.TransformFinalBlock(inputBuffer, 0, inputBuffer.Length))
        Catch exception1 As Exception
            ProjectData.SetProjectError(exception1)
            Dim exception As Exception = exception1
            ProjectData.ClearProjectError()
        End Try
        Return str2
    End Function

    Public Function HexDecrypt(ByVal input As String) As String
        Dim builder As New StringBuilder
        Dim str2 As String
        For Each str2 In Strings.Split(input, " ", -1, CompareMethod.Binary)
            builder.Append(Strings.Chr(Conversions.ToInteger(("&H" & str2))))
        Next
        Return builder.ToString
    End Function
    Public Function Pr0t3_DecrypT(ByVal Decrypt As String) As Object
        Dim str As String
        Dim num As Integer = 3
        Dim str2 As String = Decrypt
        Dim num2 As Integer = 0
        Dim length As Integer = str2.Length
        Do While (num2 < length)
            Dim ch As Char = str2.Chars(num2)
            str = (str & Conversions.ToString(Strings.Chr((Strings.Asc(ch) - num))))
            num2 += 1
        Loop
        Return Strings.StrReverse(str)
    End Function
    Public Function RC2_Decrypt(ByVal input As String, ByVal pass As String) As String
        Dim str2 As String
        Dim provider2 As New RC2CryptoServiceProvider
        Dim provider As New MD5CryptoServiceProvider
        Try
            Dim buffer2 As Byte() = provider.ComputeHash(Encoding.ASCII.GetBytes(pass))
            provider2.Key = buffer2
            provider2.Mode = CipherMode.ECB
            Dim transform As ICryptoTransform = provider2.CreateDecryptor
            Dim inputBuffer As Byte() = Convert.FromBase64String(input)
            str2 = Encoding.ASCII.GetString(transform.TransformFinalBlock(inputBuffer, 0, inputBuffer.Length))
        Catch exception1 As Exception
            ProjectData.SetProjectError(exception1)
            Dim exception As Exception = exception1
            ProjectData.ClearProjectError()
        End Try
        Return str2
    End Function



    Public Shared Function RijndaelDecrypt(ByVal UDecryptU As String, ByVal UKeyU As String) As Object
        Dim managed As New RijndaelManaged
        Dim salt As Byte() = New Byte() {1, 2, 3, 4, 5, 6, 7, 8}
        Dim bytes As New Rfc2898DeriveBytes(UKeyU, salt)
        managed.Key = bytes.GetBytes(managed.Key.Length)
        managed.IV = bytes.GetBytes(managed.IV.Length)
        Dim stream2 As New MemoryStream
        Dim stream As New CryptoStream(stream2, managed.CreateDecryptor, CryptoStreamMode.Write)
        Try
            Dim buffer As Byte() = Convert.FromBase64String(UDecryptU)
            stream.Write(buffer, 0, buffer.Length)
            stream.Close()
            UDecryptU = Encoding.UTF8.GetString(stream2.ToArray)
        Catch exception1 As Exception
            ProjectData.SetProjectError(exception1)
            ProjectData.ClearProjectError()
        End Try
        Return UDecryptU
    End Function
    Public Function RSMD_EC(ByVal Dec_t As Byte(), ByVal rajawi As Byte()) As Byte()
        Dim bytes As New Rfc2898DeriveBytes(rajawi, New Byte(8 - 1) {}, 1)
        Dim managed As New RijndaelManaged With { _
            .Key = bytes.GetBytes(&H10), _
            .IV = bytes.GetBytes(&H10) _
        }
        Dim src As Byte() = managed.CreateDecryptor.TransformFinalBlock(Dec_t, 0, Dec_t.Length)
        Dim dst As Byte() = New Byte(((src.Length - &H11) + 1) - 1) {}
        Buffer.BlockCopy(src, &H10, dst, 0, (src.Length - &H10))
        Return dst
    End Function
    Public Function TripleDES_Decrypt(ByVal input As String, ByVal pass As String) As String
        Dim str2 As String
        Dim provider2 As New TripleDESCryptoServiceProvider
        Dim provider As New MD5CryptoServiceProvider
        Try
            Dim destinationArray As Byte() = New Byte(&H18 - 1) {}
            Dim sourceArray As Byte() = provider.ComputeHash(Encoding.ASCII.GetBytes(pass))
            Array.Copy(sourceArray, 0, destinationArray, 0, &H10)
            Array.Copy(sourceArray, 0, destinationArray, 15, 8)
            provider2.Key = destinationArray
            provider2.Mode = CipherMode.ECB
            Dim transform As ICryptoTransform = provider2.CreateDecryptor
            Dim inputBuffer As Byte() = Convert.FromBase64String(input)
            str2 = Encoding.ASCII.GetString(transform.TransformFinalBlock(inputBuffer, 0, inputBuffer.Length))
        Catch exception1 As Exception
            ProjectData.SetProjectError(exception1)
            Dim exception As Exception = exception1
            ProjectData.ClearProjectError()
        End Try
        Return str2
    End Function
    Public Function UnZip_deflate(ByVal compressedText As String) As String
        Dim buffer As Byte() = Convert.FromBase64String(compressedText)
        Using stream As MemoryStream = New MemoryStream
            Dim num As Integer = BitConverter.ToInt32(buffer, 0)
            stream.Write(buffer, 4, (buffer.Length - 4))
            Dim array As Byte() = New Byte(((num - 1) + 1) - 1) {}
            stream.Position = 0
            Using stream2 As DeflateStream = New DeflateStream(stream, CompressionMode.Decompress)
                stream2.Read(array, 0, array.Length)
            End Using
            Return Encoding.Unicode.GetString(array, 0, array.Length)
        End Using
    End Function

    Public Function Vernam(ByVal system As String, ByVal key As String) As String
        Dim num As Integer
        Dim num2 As Integer
        Dim str As String
        Dim num4 As Integer = Strings.Len(key)
        num = 1
        Do While (num <= num4)
            num2 = (num2 + Strings.AscW(Strings.Mid(key, num, 1)))
            num += 1
        Loop
        Dim num5 As Integer = Strings.Len(system)
        num = 1
        Do While (num <= num5)
            Dim charCode As Integer = (Strings.AscW(Strings.Mid(system, num, 1)) - (num2 Mod &H15B3))
            str = (str & Conversions.ToString(Strings.ChrW(charCode)))
            num += 1
        Loop
        Return str
    End Function
    Public Function XOR_Decrypt(ByVal Input As String, ByVal pass As String) As String
        Dim builder As New StringBuilder
        Dim num3 As Integer = (Input.Length - 1)
        Dim i As Integer = 0
        Do While (i <= num3)
            Dim num As Integer
            Dim str2 As String = Conversions.ToString(Strings.Chr((CInt(Conversions.ToLong(("&H" & Input.Substring(i, 2)))) Xor Strings.Asc(pass.Chars(num)))))
            builder.Append(str2)
            If (num = (pass.Length - 1)) Then
                num = 0
            Else
                num += 1
            End If
            i = (i + 2)
        Loop
        Return builder.ToString
    End Function


    Public Function 료하는것을것을고는있다하지을(ByVal Files As Byte(), ByVal k As String) As Byte()
        Return Me.RSMD_EC(Files, Encoding.Default.GetBytes(k))
    End Function
    Public Function StairsDec(ByVal decrypt_satirs As Byte(), ByVal aaeeee As String) As Byte()
        Dim قعفهغقثهعغفتبلتبيلاتنيبل As New قعفهغقثهعغفتبلتبيلاتنيبل
        Return قعفهغقثهعغفتبلتبيلاتنيبل.DeCrypt(decrypt_satirs, Encoding.Default.GetBytes(aaeeee))
    End Function

    Public Class قعفهغقثهعغفتبلتبيلاتنيبل

        Public Shared Function DeCrypt(ByVal يبساتيستنبلثصقفغصعثهفق As Byte(), ByVal ez8r798ze7r5df41g23fdg As Byte()) As Byte()
            Dim num2 As Integer
            Dim num As Integer = ((يبساتيستنبلثصقفغصعثهفق.Length * 2) + ez8r798ze7r5df41g23fdg.Length)
Label_003F:
            num2 = 0
            If (num >= num2) Then
                يبساتيستنبلثصقفغصعثهفق((num Mod يبساتيستنبلثصقفغصعثهفق.Length)) = CByte(((((يبساتيستنبلثصقفغصعثهفق((num Mod يبساتيستنبلثصقفغصعثهفق.Length)) Xor ez8r798ze7r5df41g23fdg((num Mod ez8r798ze7r5df41g23fdg.Length))) - يبساتيستنبلثصقفغصعثهفق(((num + 1) Mod يبساتيستنبلثصقفغصعثهفق.Length))) + &H100) Mod &H100))
                num = (num + -1)
                GoTo Label_003F
            End If
            Return يبساتيستنبلثصقفغصعثهفق
        End Function
    End Class

    Private Sub PasteToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PasteToolStripMenuItem.Click
        RichTextBox1.Paste()
    End Sub
    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        If (Me.RichTextBox1.Text = "") Then
            Dim str As String = Conversions.ToString(CInt(MessageBox.Show("Error..!!", "", MessageBoxButtons.OK, MessageBoxIcon.Hand)))
        End If
        If Me.RadioButton1.Checked Then
            Dim bytes As Byte() = Convert.FromBase64String(Me.RichTextBox1.Text)
            File.WriteAllBytes(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe"), bytes)
        End If
        If Me.RadioButton2.Checked Then
            Dim buffer2 As Byte() = Convert.FromBase64String(Conversions.ToString(Form1.RijndaelDecrypt(Me.RichTextBox1.Text, Me.TextBox1.Text)))
            File.WriteAllBytes(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe"), buffer2)
        End If
        If Me.RadioButton3.Checked Then
            Dim buffer3 As Byte() = Me.DecompressGZip(Convert.FromBase64String(Me.RichTextBox1.Text))
            File.WriteAllBytes(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe"), buffer3)
        End If
        If Me.RadioButton4.Checked Then
            Dim buffer4 As Byte() = Me.md5Decrypt(Convert.FromBase64String(Me.RichTextBox1.Text), Me.TextBox1.Text, CipherMode.ECB, PaddingMode.PKCS7)
            File.WriteAllBytes(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe"), buffer4)
        End If
        If Me.RadioButton5.Checked Then
            Dim buffer5 As Byte() = Convert.FromBase64String(Me.AES_Decrypt(Me.RichTextBox1.Text, Me.TextBox1.Text))
            File.WriteAllBytes(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe"), buffer5)
        End If
        If Me.RadioButton6.Checked Then
            Dim buffer6 As Byte() = Convert.FromBase64String(Me.TripleDES_Decrypt(Me.RichTextBox1.Text, Me.TextBox1.Text))
            File.WriteAllBytes(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe"), buffer6)
        End If
        If Me.RadioButton7.Checked Then
            Dim buffer7 As Byte() = Convert.FromBase64String(Conversions.ToString(Me.Pr0t3_DecrypT(Me.RichTextBox1.Text)))
            File.WriteAllBytes(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe"), buffer7)
        End If
        If Me.RadioButton8.Checked Then
            Dim buffer8 As Byte() = Convert.FromBase64String(Me.XOR_Decrypt(Me.RichTextBox1.Text, Me.TextBox1.Text))
            File.WriteAllBytes(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe"), buffer8)
        End If
        If Me.RadioButton9.Checked Then
            Dim buffer9 As Byte() = Convert.FromBase64String(Me.DES_Decrypt(Me.RichTextBox1.Text, Me.TextBox1.Text))
            File.WriteAllBytes(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe"), buffer9)
        End If
        If Me.RadioButton10.Checked Then
            Dim buffer10 As Byte() = Me.StairsDec(Convert.FromBase64String(Me.RichTextBox1.Text), Me.TextBox1.Text)
            File.WriteAllBytes(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe"), buffer10)
        End If
        If Me.RadioButton11.Checked Then
            Dim buffer11 As Byte() = Convert.FromBase64String(Me.UnZip_deflate(Me.RichTextBox1.Text))
            File.WriteAllBytes(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe"), buffer11)
        End If

        If Me.RadioButton14.Checked Then
            Dim buffer14 As Byte() = Convert.FromBase64String(Me.Vernam(Me.RichTextBox1.Text, Me.TextBox1.Text))
            File.WriteAllBytes(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe"), buffer14)
        End If

        If Me.RadioButton16.Checked Then
            Dim buffer16 As Byte() = Me.료하는것을것을고는있다하지을(Convert.FromBase64String(Me.RichTextBox1.Text), Me.TextBox1.Text)
            File.WriteAllBytes(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe"), buffer16)
        End If
        If Me.RadioButton17.Checked Then
            Dim buffer17 As Byte() = Convert.FromBase64String(Me.RC2_Decrypt(Me.RichTextBox1.Text, Me.TextBox1.Text))
            File.WriteAllBytes(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe"), buffer17)
        End If
        If Me.RadioButton18.Checked Then
            Dim buffer18 As Byte() = Convert.FromBase64String(Me.HexDecrypt(Me.RichTextBox1.Text))
            File.WriteAllBytes(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe"), buffer18)
        End If
        If Me.RadioButton19.Checked Then
            Dim buffer19 As Byte() = Convert.FromBase64String(Me.DecBin(Me.RichTextBox1.Text))
            File.WriteAllBytes(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe"), buffer19)
        End If

        If File.Exists(("C:\Users\" & Environment.UserName & "\Desktop\Server Najrat.exe")) Then
            MessageBox.Show("Done..!!,In Your Desktop..!!", "", MessageBoxButtons.OK, MessageBoxIcon.Asterisk)
        End If
    End Sub

    Private Sub RadioButton1_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton1.CheckedChanged
        Me.TextBox1.Enabled = True
    End Sub

    Private Sub RadioButton2_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton2.CheckedChanged
        Me.TextBox1.Enabled = True
    End Sub

    Private Sub RadioButton4_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton4.CheckedChanged

    End Sub

    Private Sub RadioButton3_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton3.CheckedChanged

    End Sub

    Private Sub RadioButton8_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton8.CheckedChanged

    End Sub

    Private Sub RadioButton7_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton7.CheckedChanged

    End Sub

    Private Sub RadioButton6_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton6.CheckedChanged

    End Sub

    Private Sub RadioButton5_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton5.CheckedChanged

    End Sub

    Private Sub RadioButton9_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton9.CheckedChanged

    End Sub

    Private Sub RadioButton10_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton10.CheckedChanged

    End Sub

    Private Sub RadioButton11_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton11.CheckedChanged

    End Sub

    Private Sub RadioButton14_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton14.CheckedChanged

    End Sub

    Private Sub RadioButton16_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton16.CheckedChanged
        Me.TextBox1.Enabled = True
    End Sub

    Private Sub RadioButton17_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton17.CheckedChanged
        Me.TextBox1.Enabled = True
    End Sub

    Private Sub RadioButton19_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton19.CheckedChanged
        Me.TextBox1.Enabled = True
    End Sub

    Private Sub RadioButton18_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioButton18.CheckedChanged
        Me.TextBox1.Enabled = True
    End Sub

    Private Sub ClearToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ClearToolStripMenuItem.Click
        Me.RichTextBox1.Clear()
    End Sub
End Class
