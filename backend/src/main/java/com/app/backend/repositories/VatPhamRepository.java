package com.app.backend.repositories;

import com.app.backend.models.VatPham;
import com.app.backend.models.enums.LoaiVatPham;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface VatPhamRepository extends JpaRepository<VatPham, Long> {

    /**
     * Tìm vật phẩm theo loại
     */
    Optional<VatPham> findByLoai(LoaiVatPham loai);

    /**
     * Lấy tất cả vật phẩm đang active
     */
    List<VatPham> findByKichHoatTrue();

    /**
     * Lấy vật phẩm theo độ hiếm
     */
    List<VatPham> findByDoHiemAndKichHoatTrue(String doHiem);

    /**
     * Lấy vật phẩm trong khoảng giá xu
     */
    @Query("SELECT v FROM VatPham v WHERE v.kichHoat = true AND v.giaXu BETWEEN :minPrice AND :maxPrice")
    List<VatPham> findByPriceRange(Integer minPrice, Integer maxPrice);

    /**
     * Kiểm tra tồn tại theo loại
     */
    boolean existsByLoai(LoaiVatPham loai);
}
