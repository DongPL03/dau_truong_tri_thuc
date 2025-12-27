package com.app.backend.repositories;

import com.app.backend.models.SuDungVatPhamTranDau;
import com.app.backend.models.enums.LoaiVatPham;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface SuDungVatPhamTranDauRepository extends JpaRepository<SuDungVatPhamTranDau, Long> {

    /**
     * Lấy lịch sử sử dụng vật phẩm trong một trận đấu
     */
    List<SuDungVatPhamTranDau> findByTranDauId(Long tranDauId);

    /**
     * Lấy lịch sử sử dụng vật phẩm của một user trong trận
     */
    List<SuDungVatPhamTranDau> findByTranDauIdAndNguoiDungId(Long tranDauId, Long nguoiDungId);

    /**
     * Đếm số lần sử dụng loại vật phẩm cụ thể trong trận
     */
    @Query("SELECT COUNT(s) FROM SuDungVatPhamTranDau s " +
            "WHERE s.tranDau.id = :tranDauId AND s.nguoiDung.id = :userId AND s.loaiVatPham = :loai")
    int countUsageInBattle(@Param("tranDauId") Long tranDauId,
                           @Param("userId") Long userId,
                           @Param("loai") LoaiVatPham loai);

    /**
     * Kiểm tra user đã dùng loại vật phẩm này trong trận chưa (giới hạn 1 lần/trận)
     */
    boolean existsByTranDauIdAndNguoiDungIdAndLoaiVatPham(Long tranDauId, Long nguoiDungId, LoaiVatPham loai);

    /**
     * Thống kê vật phẩm được dùng nhiều nhất
     */
    @Query("SELECT s.loaiVatPham, COUNT(s) as cnt FROM SuDungVatPhamTranDau s " +
            "GROUP BY s.loaiVatPham ORDER BY cnt DESC")
    List<Object[]> getItemUsageStats();
}
