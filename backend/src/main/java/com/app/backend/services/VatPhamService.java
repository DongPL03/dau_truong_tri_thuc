package com.app.backend.services;

import com.app.backend.dtos.SuDungVatPhamDTO;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.models.*;
import com.app.backend.models.enums.LoaiVatPham;
import com.app.backend.repositories.*;
import com.app.backend.responses.SuDungVatPhamResponse;
import com.app.backend.responses.VatPhamInventoryResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class VatPhamService {

    private final VatPhamRepository vatPhamRepository;
    private final VatPhamNguoiDungRepository vatPhamNguoiDungRepository;
    private final SuDungVatPhamTranDauRepository suDungVatPhamTranDauRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final ITranDauRepository tranDauRepository;

    /**
     * Lấy danh sách inventory của user
     */
    public List<VatPhamInventoryResponse> getInventory(Long userId) {
        List<VatPhamNguoiDung> items = vatPhamNguoiDungRepository.findAvailableByUserId(userId);
        return items.stream()
                .map(this::toInventoryResponse)
                .toList();
    }

    /**
     * Lấy tất cả vật phẩm đang active (cho shop/display)
     */
    public List<VatPham> getAllActiveItems() {
        return vatPhamRepository.findByKichHoatTrue();
    }

    /**
     * Thêm vật phẩm cho user (khi thắng trận, nhận thưởng...)
     */
    @Transactional
    public void grantItemToUser(Long userId, Long vatPhamId, int quantity) throws DataNotFoundException {
        NguoiDung user = nguoiDungRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy người dùng"));
        VatPham vatPham = vatPhamRepository.findById(vatPhamId)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy vật phẩm"));

        Optional<VatPhamNguoiDung> existing = vatPhamNguoiDungRepository
                .findByNguoiDungIdAndVatPhamId(userId, vatPhamId);

        if (existing.isPresent()) {
            VatPhamNguoiDung inv = existing.get();
            inv.setSoLuong(inv.getSoLuong() + quantity);
            inv.setNhanLuc(LocalDateTime.now());
            vatPhamNguoiDungRepository.save(inv);
        } else {
            VatPhamNguoiDung newInv = VatPhamNguoiDung.builder()
                    .nguoiDung(user)
                    .vatPham(vatPham)
                    .soLuong(quantity)
                    .nhanLuc(LocalDateTime.now())
                    .build();
            vatPhamNguoiDungRepository.save(newInv);
        }

        log.info("Granted {} x {} to user {}", quantity, vatPham.getTen(), userId);
    }

    /**
     * Thêm vật phẩm theo loại
     */
    @Transactional
    public void grantItemByType(Long userId, LoaiVatPham loai, int quantity) throws DataNotFoundException {
        VatPham vatPham = vatPhamRepository.findByLoai(loai)
                .orElseThrow(() -> new DataNotFoundException("Không tìm thấy vật phẩm loại " + loai));
        grantItemToUser(userId, vatPham.getId(), quantity);
    }

    /**
     * Sử dụng vật phẩm trong trận đấu
     */
    @Transactional
    public SuDungVatPhamResponse useItem(Long userId, SuDungVatPhamDTO dto, BattleState battleState)
            throws DataNotFoundException {

        // Xác định vật phẩm từ ID hoặc loại
        VatPham vatPham;
        if (dto.getVatPhamId() != null) {
            vatPham = vatPhamRepository.findById(dto.getVatPhamId())
                    .orElseThrow(() -> new DataNotFoundException("Không tìm thấy vật phẩm"));
        } else if (dto.getLoaiVatPham() != null) {
            vatPham = vatPhamRepository.findByLoai(dto.getLoaiVatPham())
                    .orElseThrow(() -> new DataNotFoundException("Không tìm thấy vật phẩm loại " + dto.getLoaiVatPham()));
        } else {
            throw new IllegalArgumentException("Phải cung cấp vat_pham_id hoặc loai_vat_pham");
        }

        // Kiểm tra inventory
        VatPhamNguoiDung inventory = vatPhamNguoiDungRepository
                .findByNguoiDungIdAndVatPhamId(userId, vatPham.getId())
                .orElseThrow(() -> new DataNotFoundException("Bạn không có vật phẩm này"));

        if (inventory.getSoLuong() <= 0) {
            return SuDungVatPhamResponse.builder()
                    .thanhCong(false)
                    .thongBao("Bạn đã hết vật phẩm " + vatPham.getTen())
                    .build();
        }

        // Kiểm tra giới hạn sử dụng trong trận (một số item chỉ dùng 1 lần/trận)
        if (isLimitedPerBattle(vatPham.getLoai())) {
            boolean alreadyUsed = suDungVatPhamTranDauRepository
                    .existsByTranDauIdAndNguoiDungIdAndLoaiVatPham(dto.getTranDauId(), userId, vatPham.getLoai());
            if (alreadyUsed) {
                return SuDungVatPhamResponse.builder()
                        .thanhCong(false)
                        .loaiVatPham(vatPham.getLoai())
                        .thongBao("Vật phẩm này chỉ được dùng 1 lần trong trận!")
                        .build();
            }
        }

        // Áp dụng hiệu ứng
        SuDungVatPhamResponse.HieuUngVatPham hieuUng = applyItemEffect(vatPham, battleState, userId);

        // Trừ số lượng
        inventory.setSoLuong(inventory.getSoLuong() - 1);
        inventory.setSuDungLuc(LocalDateTime.now());
        vatPhamNguoiDungRepository.save(inventory);

        // Ghi lịch sử
        TranDau tranDau = tranDauRepository.findById(dto.getTranDauId()).orElse(null);
        NguoiDung user = nguoiDungRepository.findById(userId).orElse(null);

        if (tranDau != null && user != null) {
            SuDungVatPhamTranDau lichSu = SuDungVatPhamTranDau.builder()
                    .tranDau(tranDau)
                    .nguoiDung(user)
                    .vatPham(vatPham)
                    .loaiVatPham(vatPham.getLoai())
                    .cauHoiIndex(dto.getCauHoiIndex())
                    .suDungLuc(LocalDateTime.now())
                    .ketQua(hieuUng.toString())
                    .build();
            suDungVatPhamTranDauRepository.save(lichSu);
        }

        log.info("User {} used item {} in battle {}", userId, vatPham.getTen(), dto.getTranDauId());

        return SuDungVatPhamResponse.builder()
                .thanhCong(true)
                .loaiVatPham(vatPham.getLoai())
                .tenVatPham(vatPham.getTen())
                .thongBao("Đã sử dụng " + vatPham.getTen() + " thành công!")
                .hieuUng(hieuUng)
                .soLuongConLai(inventory.getSoLuong())
                .build();
    }

    /**
     * Áp dụng hiệu ứng vật phẩm vào BattleState
     */
    private SuDungVatPhamResponse.HieuUngVatPham applyItemEffect(VatPham vatPham, BattleState state, Long userId) {
        SuDungVatPhamResponse.HieuUngVatPham.HieuUngVatPhamBuilder builder =
                SuDungVatPhamResponse.HieuUngVatPham.builder();

        switch (vatPham.getLoai()) {
            case X2_DIEM:
                // Đánh dấu người chơi có x2 điểm cho câu tiếp theo
                state.getActiveMultipliers().put(userId, 2.0);
                builder.heSoDiem(2.0);
                break;

            case X3_DIEM:
                state.getActiveMultipliers().put(userId, 3.0);
                builder.heSoDiem(3.0);
                break;

            case DONG_BANG_THOI_GIAN:
                // Thêm 5 giây cho câu hiện tại (xử lý ở frontend)
                int extraTime = vatPham.getThoiGianHieuLucGiay() != null ? vatPham.getThoiGianHieuLucGiay() : 5;
                state.getExtraTimeSeconds().merge(userId, extraTime, Integer::sum);
                builder.thoiGianThemGiay(extraTime);
                break;

            case GOI_Y_50_50:
                // Loại bỏ 2 đáp án sai
                CauHoi currentQuestion = state.getCurrentQuestion();
                if (currentQuestion != null) {
                    List<String> wrongAnswers = getWrongAnswers(currentQuestion);
                    Collections.shuffle(wrongAnswers);
                    List<String> toRemove = wrongAnswers.subList(0, Math.min(2, wrongAnswers.size()));
                    state.getEliminatedOptions().put(userId, new HashSet<>(toRemove));
                    builder.dapAnBiLoai(toRemove);
                }
                break;

            case KHIEN_BAO_VE:
                // Bảo vệ combo cho câu tiếp theo
                state.getShieldedPlayers().add(userId);
                builder.baoVeCombo(true);
                break;

            case BO_QUA_CAU_HOI:
                // Đánh dấu bỏ qua câu này, không tính điểm
                state.getSkippedQuestions().computeIfAbsent(userId, k -> new HashSet<>())
                        .add(state.getCurrentQuestionIndex());
                builder.boQuaThanhCong(true);
                break;

            case HIEN_DAP_AN:
                // Hiển thị đáp án đúng (rất hiếm)
                CauHoi q = state.getCurrentQuestion();
                if (q != null) {
                    builder.dapAnDung(String.valueOf(q.getDapAnDung()));
                }
                break;
        }

        return builder.build();
    }

    /**
     * Lấy các đáp án sai của câu hỏi
     */
    private List<String> getWrongAnswers(CauHoi cauHoi) {
        List<String> wrong = new ArrayList<>();
        String correct = String.valueOf(cauHoi.getDapAnDung());
        for (String opt : Arrays.asList("A", "B", "C", "D")) {
            if (!opt.equals(correct)) {
                wrong.add(opt);
            }
        }
        return wrong;
    }

    /**
     * Kiểm tra vật phẩm có giới hạn 1 lần/trận không
     */
    private boolean isLimitedPerBattle(LoaiVatPham loai) {
        return loai == LoaiVatPham.HIEN_DAP_AN || loai == LoaiVatPham.X3_DIEM;
    }

    /**
     * Thưởng vật phẩm ngẫu nhiên sau trận đấu
     */
    @Transactional
    public VatPham rewardRandomItem(Long userId, boolean isWinner) throws DataNotFoundException {
        List<VatPham> activeItems = vatPhamRepository.findByKichHoatTrue();
        if (activeItems.isEmpty()) return null;

        // Xác suất nhận item cao hơn nếu thắng
        double chance = isWinner ? 0.4 : 0.15;
        if (Math.random() > chance) return null;

        // Weighted random theo độ hiếm
        List<VatPham> eligibleItems = new ArrayList<>();
        for (VatPham item : activeItems) {
            int weight = switch (item.getDoHiem()) {
                case "LEGENDARY" -> 1;
                case "EPIC" -> 3;
                case "RARE" -> 6;
                default -> 10; // COMMON
            };
            for (int i = 0; i < weight; i++) {
                eligibleItems.add(item);
            }
        }

        if (eligibleItems.isEmpty()) return null;

        VatPham selected = eligibleItems.get(new Random().nextInt(eligibleItems.size()));
        grantItemToUser(userId, selected.getId(), 1);
        return selected;
    }

    /**
     * Khởi tạo vật phẩm mặc định (chạy khi startup)
     */
    @Transactional
    public void initDefaultItems() {
        if (vatPhamRepository.count() > 0) return;

        List<VatPham> defaults = Arrays.asList(
                VatPham.builder()
                        .ten("Nhân đôi điểm")
                        .moTa("Nhân đôi điểm cho câu trả lời đúng tiếp theo")
                        .loai(LoaiVatPham.X2_DIEM)
                        .giaTriHieuUng(2.0)
                        .icon("⚡")
                        .mauSac("#FFD700")
                        .doHiem("COMMON")
                        .giaXu(100)
                        .build(),

                VatPham.builder()
                        .ten("Đóng băng thời gian")
                        .moTa("Dừng đồng hồ thêm 5 giây")
                        .loai(LoaiVatPham.DONG_BANG_THOI_GIAN)
                        .thoiGianHieuLucGiay(5)
                        .icon("❄️")
                        .mauSac("#00BFFF")
                        .doHiem("COMMON")
                        .giaXu(80)
                        .build(),

                VatPham.builder()
                        .ten("Gợi ý 50/50")
                        .moTa("Loại bỏ 2 đáp án sai")
                        .loai(LoaiVatPham.GOI_Y_50_50)
                        .giaTriHieuUng(2.0)
                        .icon("🎯")
                        .mauSac("#9932CC")
                        .doHiem("RARE")
                        .giaXu(150)
                        .build(),

                VatPham.builder()
                        .ten("Khiên bảo vệ")
                        .moTa("Bảo vệ combo khi trả lời sai 1 lần")
                        .loai(LoaiVatPham.KHIEN_BAO_VE)
                        .icon("🛡️")
                        .mauSac("#228B22")
                        .doHiem("RARE")
                        .giaXu(120)
                        .build(),

                VatPham.builder()
                        .ten("Bỏ qua câu hỏi")
                        .moTa("Bỏ qua câu hỏi hiện tại mà không mất điểm hay combo")
                        .loai(LoaiVatPham.BO_QUA_CAU_HOI)
                        .icon("⏭️")
                        .mauSac("#FF6347")
                        .doHiem("EPIC")
                        .giaXu(200)
                        .build(),

                VatPham.builder()
                        .ten("Nhân ba điểm")
                        .moTa("Nhân ba điểm cho câu trả lời đúng tiếp theo (1 lần/trận)")
                        .loai(LoaiVatPham.X3_DIEM)
                        .giaTriHieuUng(3.0)
                        .icon("💎")
                        .mauSac("#E6E6FA")
                        .doHiem("EPIC")
                        .giaXu(300)
                        .build(),

                VatPham.builder()
                        .ten("Tiết lộ đáp án")
                        .moTa("Hiển thị đáp án đúng (cực hiếm, 1 lần/trận)")
                        .loai(LoaiVatPham.HIEN_DAP_AN)
                        .icon("👁️")
                        .mauSac("#FF1493")
                        .doHiem("LEGENDARY")
                        .giaXu(500)
                        .build()
        );

        vatPhamRepository.saveAll(defaults);
        log.info("Initialized {} default items", defaults.size());
    }

    private VatPhamInventoryResponse toInventoryResponse(VatPhamNguoiDung inv) {
        VatPham vp = inv.getVatPham();
        return VatPhamInventoryResponse.builder()
                .vatPhamId(vp.getId())
                .ten(vp.getTen())
                .moTa(vp.getMoTa())
                .loai(vp.getLoai())
                .icon(vp.getIcon())
                .mauSac(vp.getMauSac())
                .doHiem(vp.getDoHiem())
                .soLuong(inv.getSoLuong())
                .giaTriHieuUng(vp.getGiaTriHieuUng())
                .thoiGianHieuLucGiay(vp.getThoiGianHieuLucGiay())
                .build();
    }
}
