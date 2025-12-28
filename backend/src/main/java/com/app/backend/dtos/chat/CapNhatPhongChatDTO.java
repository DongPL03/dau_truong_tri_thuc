package com.app.backend.dtos.chat;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CapNhatPhongChatDTO {

    private String ten; // Đổi tên nhóm

    private String anhNhom; // Đổi ảnh đại diện

    private List<Long> themThanhVien; // Thêm thành viên mới

    private List<Long> xoaThanhVien; // Xóa thành viên
}
