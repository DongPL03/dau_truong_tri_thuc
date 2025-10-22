package com.app.backend.models;


import com.app.backend.models.enums.DoKho;
import com.app.backend.models.enums.LoaiNoiDung;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "cau_hoi")
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CauHoi {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "bo_cau_hoi_id", nullable = false)
    private BoCauHoi boCauHoi;

    @Enumerated(EnumType.STRING)
    @Column(name = "do_kho", columnDefinition = "ENUM('DE','TRUNG_BINH','KHO') DEFAULT 'TRUNG_BINH'")
    private DoKho doKho = DoKho.TRUNG_BINH;

    @Lob
    @Column(name = "noi_dung", nullable = false)
    private String noiDung;

    @Enumerated(EnumType.STRING)
    @Column(name = "loai_noi_dung", columnDefinition = "ENUM('VAN_BAN','HINH_ANH','AM_THANH','VIDEO') DEFAULT 'VAN_BAN'")
    private LoaiNoiDung loaiNoiDung = LoaiNoiDung.VAN_BAN;

    @Column(name = "duong_dan_tep")
    private String duongDanTep;

    @Column(name = "lua_chon_a", nullable = false)
    private String luaChonA;

    @Column(name = "lua_chon_b", nullable = false)
    private String luaChonB;

    @Column(name = "lua_chon_c", nullable = false)
    private String luaChonC;

    @Column(name = "lua_chon_d", nullable = false)
    private String luaChonD;

    @Column(name = "dap_an_dung", length = 1, nullable = false)
    private Character dapAnDung; // A, B, C, D

    @Lob
    @Column(name = "giai_thich")
    private String giaiThich;

    @Column(name = "cap_nhat_luc")
    private LocalDateTime capNhatLuc;

    @PreUpdate
    protected void onUpdate() {
        capNhatLuc = LocalDateTime.now();
    }
}


