package com.app.backend.dtos.chat;

import com.app.backend.models.enums.LoaiTinNhan;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class GuiTinNhanDTO {

    @NotNull(message = "Phòng chat không được để trống")
    private Long phongChatId;

    private String noiDung; // Nội dung text

    private LoaiTinNhan loai = LoaiTinNhan.VAN_BAN;

    private String urlMedia; // URL cho ảnh, file đã upload

    private String tenFile; // Tên file gốc

    private Long kichThuocFile; // Kích thước file

    private Long traLoiChoId; // ID tin nhắn đang trả lời
}
