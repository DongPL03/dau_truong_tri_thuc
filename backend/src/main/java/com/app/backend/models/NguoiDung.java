package com.app.backend.models;

import com.app.backend.models.constant.TrangThaiNguoiDung;
import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.persistence.*;
import lombok.*;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

@Entity
@Table(name = "nguoi_dung")
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class NguoiDung implements UserDetails {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "ten_dang_nhap", length = 50, nullable = false, unique = true)
    private String tenDangNhap;

    @Column(name = "email", length = 100, nullable = false, unique = true)
    private String email;

    @Column(name = "ho_ten", length = 100, nullable = false)
    private String hoTen;

    @Column(name = "dia_chi", length = 200, nullable = false)
    private String diaChi;

    @Column(name = "mat_khau", nullable = false)
    private String password;

    @Column(name = "is_active")
    private boolean active;

    @Column(name = "is_xoa", nullable = false)
    private boolean isXoa = false;

    @Column(name = "ten_hien_thi", length = 100)
    private String tenHienThi;

    @Column(name = "avatar_url")
    private String avatarUrl;

    // Thay thế ENUM bằng khóa ngoại
    @ManyToOne(fetch = FetchType.EAGER)
    @JsonBackReference
    @JoinColumn(name = "vai_tro_id", nullable = true) // BIGINT (có thể là NULL)
    private VaiTro vaiTro;


    @Column(name = "trang_thai", columnDefinition = "ENUM('ONLINE','OFFLINE','BANNED') DEFAULT 'OFFLINE'")
    private String trangThai = TrangThaiNguoiDung.OFFLINE;


    @Column(name = "dang_nhap_lan_cuoi")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant dangNhapLanCuoi;

    @Column(name = "tao_luc", updatable = false)
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant taoLuc;

    @Column(name = "cap_nhat_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant capNhatLuc;

    @PrePersist
    protected void onCreate() {
        taoLuc = Instant.now();
        capNhatLuc = Instant.now();
    }

    @PreUpdate
    protected void onUpdate() {
        capNhatLuc = Instant.now();
    }

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        List<SimpleGrantedAuthority> authorityList = new ArrayList<>();
        authorityList.add(new SimpleGrantedAuthority("ROLE_" + getVaiTro().getTenVaiTro().toUpperCase()));
        return authorityList;
    }

    @Override
    public String getUsername() {
        if (tenDangNhap != null && !tenDangNhap.isEmpty()) {
            return tenDangNhap;
        } else if (email != null && !email.isEmpty()) {
            return email;
        }
        return "";
    }


    @Override
    public boolean isAccountNonExpired() {
        return true;
    }

    @Override
    public boolean isAccountNonLocked() {
        return true;
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return true;
    }

    @Override
    public boolean isEnabled() {
        return true;
    }
}

