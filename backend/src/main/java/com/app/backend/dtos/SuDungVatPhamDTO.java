package com.app.backend.dtos;

import com.app.backend.models.enums.LoaiVatPham;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

/**
 * DTO để sử dụng vật phẩm trong trận đấu
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SuDungVatPhamDTO {

    /**
     * ID trận đấu
     */
    @JsonProperty("tran_dau_id")
    private Long tranDauId;

    /**
     * ID vật phẩm muốn sử dụng
     */
    @JsonProperty("vat_pham_id")
    private Long vatPhamId;

    /**
     * Hoặc có thể dùng loại vật phẩm thay vì ID
     */
    @JsonProperty("loai_vat_pham")
    private LoaiVatPham loaiVatPham;

    /**
     * Index câu hỏi hiện tại (để validate)
     */
    @JsonProperty("cau_hoi_index")
    private Integer cauHoiIndex;
}
