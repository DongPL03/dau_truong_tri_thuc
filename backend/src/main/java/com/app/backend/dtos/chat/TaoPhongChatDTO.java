package com.app.backend.dtos.chat;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TaoPhongChatDTO {

    @Size(max = 100, message = "Tên phòng chat không được quá 100 ký tự")
    private String ten; // Tên nhóm (cho group chat)

    private List<Long> thanhVienIds; // Danh sách ID thành viên

    private String anhNhom; // Ảnh đại diện nhóm

    // For 1-1 chat, chỉ cần 1 thanhVienId
    // For group chat, cần >= 2 thanhVienIds
}
