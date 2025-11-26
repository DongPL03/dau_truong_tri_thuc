package com.app.backend.services.trandau;

import com.app.backend.components.BattleLoopTask;
import com.app.backend.components.BattleStateManager;
import com.app.backend.components.BattleWsPublisher;
import com.app.backend.dtos.*;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.PermissionDenyException;
import com.app.backend.models.*;
import com.app.backend.models.constant.CheDoHienThi;
import com.app.backend.models.constant.LuatTinhDiem;
import com.app.backend.models.constant.TrangThaiBoCauHoi;
import com.app.backend.models.constant.TrangThaiTranDau;
import com.app.backend.repositories.*;
import com.app.backend.responses.lichsutrandau.LichSuTranDauResponse;
import com.app.backend.responses.trandau.*;
import com.app.backend.responses.websocket.FinishedEvent;
import com.app.backend.responses.websocket.LeaderboardUpdateEvent;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class TranDauService implements ITranDauService {

    private final ITranDauRepository tranDauRepository;
    private final INguoiChoiTranDauRepository nguoiChoiTranDauRepository;
    private final IBoCauHoiRepository boCauHoiRepository;
    private final ICauHoiRepository cauHoiRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final BattleStateManager battleStateManager;
    private final BattleLoopTask battleLoopTask;
    private final BattleWsPublisher wsPublisher;
    private final ITraLoiTranDauRepository traLoiTranDauRepository;
    private final ILichSuTranDauRepository lichSuTranDauRepository;
    private final IBangXepHangRepository bangXepHangRepository;
    private final IThanhTichBoCauHoiRepository thanhTichBoCauHoiRepository;


    /**
     * Tạo mã phòng ngẫu nhiên
     */
    private String generateRoomCode(int length) {
        String chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
        Random random = new Random();
        StringBuilder sb = new StringBuilder(length);
        for (int i = 0; i < length; i++)
            sb.append(chars.charAt(random.nextInt(chars.length())));
        return sb.toString();
    }

    @Transactional
    @Override
    public TranDau taoPhong(TaoTranDauDTO taoTranDauDTO, Long currentUserId) throws Exception {
        NguoiDung host = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        BoCauHoi bo = boCauHoiRepository.findById(taoTranDauDTO.getBoCauHoiId())
                .orElseThrow(() -> new DataNotFoundException("Bộ câu hỏi không tồn tại"));

        if (Boolean.FALSE.equals(taoTranDauDTO.getCongKhai()) &&
                (taoTranDauDTO.getMaPin() == null || taoTranDauDTO.getMaPin().isBlank())) {
            throw new IllegalArgumentException("Phòng riêng tư bắt buộc có mã PIN");
        }
        // Chỉ cho phép tạo trận với bộ câu hỏi Official
        if (!Boolean.TRUE.equals(bo.getIsOfficial())) {
            throw new IllegalArgumentException("Bộ câu hỏi này không được đánh dấu Official để dùng cho thi đấu");
        }

        // Bắt buộc bộ đã được duyệt
        if (!TrangThaiBoCauHoi.DA_DUYET.equals(bo.getTrangThai())) {
            throw new IllegalArgumentException("Bộ câu hỏi này chưa được duyệt, không thể dùng để thi đấu");
        }

        // Đảm bảo bộ Official luôn ở chế độ PRIVATE (ẩn đề)
        if (!CheDoHienThi.PRIVATE.equals(bo.getCheDoHienThi())) {
            throw new IllegalArgumentException("Bộ câu hỏi Official phải ở chế độ PRIVATE");
        }

        if (taoTranDauDTO.getGioiHanNguoiChoi() < 2 || taoTranDauDTO.getGioiHanNguoiChoi() > 4) {
            throw new IllegalArgumentException("Giới hạn người chơi phải từ 2 – 4");
        }
        TranDau tranDau = new TranDau();
        tranDau.setTenPhong(taoTranDauDTO.getTenPhong());
        tranDau.setBoCauHoi(bo);
        tranDau.setChuPhong(host);
        tranDau.setCongKhai(taoTranDauDTO.getCongKhai());
        tranDau.setMaPin(taoTranDauDTO.getCongKhai() ? null : taoTranDauDTO.getMaPin());
        tranDau.setMaPhong(generateRoomCode(6));
        tranDau.setGioiHanNguoiChoi(taoTranDauDTO.getGioiHanNguoiChoi());
        tranDau.setGioiHanThoiGianCauGiay(taoTranDauDTO.getGioiHanThoiGianCauGiay());
        // Luật tính điểm nếu có enum:
        tranDau.setLuatTinhDiem(taoTranDauDTO.getLuatTinhDiem());
        tranDau.setTrangThai(TrangThaiTranDau.PENDING);

        TranDau saved = tranDauRepository.save(tranDau);

        // tự động cho host vào phòng
        nguoiChoiTranDauRepository.save(
                NguoiChoiTranDau.builder().tranDau(saved).nguoiDung(host).build()
        );

        return saved;
    }

    @Transactional
    @Override
    public TranDau thamGia(ThamGiaTranDauDTO thamGiaTranDauDTO, Long currentUserId) throws Exception {
        TranDau tranDau = tranDauRepository.findById(thamGiaTranDauDTO.getTranDauId())
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        if (!Objects.equals(tranDau.getTrangThai(), TrangThaiTranDau.PENDING)) {
            throw new IllegalStateException("Phòng không ở trạng thái chờ");
        }

        long current = nguoiChoiTranDauRepository.countByTranDau_Id(tranDau.getId());
        if (current >= tranDau.getGioiHanNguoiChoi()) {
            throw new IllegalStateException("Phòng đã đủ người");
        }

        // 🔒 Nếu phòng private → yêu cầu mã PIN
        if (Boolean.FALSE.equals(tranDau.getCongKhai())) {
            if (thamGiaTranDauDTO.getMaPin() == null || !thamGiaTranDauDTO.getMaPin().equals(tranDau.getMaPin())) {
                throw new SecurityException("Sai mã PIN hoặc phòng này riêng tư");
            }
        }

        // đã tham gia chưa?
        boolean existed = nguoiChoiTranDauRepository
                .findByTranDau_IdAndNguoiDung_Id(tranDau.getId(), currentUserId)
                .isPresent();
        if (existed) return tranDau; // idempotent

        NguoiDung user = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        nguoiChoiTranDauRepository.save(
                NguoiChoiTranDau.builder().tranDau(tranDau).nguoiDung(user).build()
        );

        // Phát WS
        int soNguoi = (int) nguoiChoiTranDauRepository.countByTranDau_Id(tranDau.getId());
        if (soNguoi > tranDau.getGioiHanNguoiChoi()) {
            throw new IllegalStateException("Phòng đã đủ số lượng người chơi.");
        }
        wsPublisher.publishPlayerJoined(tranDau.getId(), user.getId(), user.getHoTen(), soNguoi);

        // Phát bảng xếp hạng rỗng
        updateAndBroadcastLeaderboard(tranDau.getId(), null);

        return tranDau;
    }

    @Transactional
    @Override
    public void roiPhong(RoiTranDauDTO dto, Long currentUserId) throws Exception {
        TranDau tranDau = tranDauRepository.findById(dto.getTranDauId())
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        NguoiDung user = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        if (tranDau.getTrangThai() == TrangThaiTranDau.ONGOING
                && tranDau.getChuPhong() != null
                && tranDau.getChuPhong().getId().equals(user.getId())) {
            throw new IllegalStateException("Chủ phòng không thể rời phòng khi trận đang diễn ra. Hãy kết thúc trận trước.");
        }

        NguoiChoiTranDau nctd = nguoiChoiTranDauRepository
                .findByTranDau_IdAndNguoiDung_Id(tranDau.getId(), currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Bạn chưa ở trong phòng"));
        // Nếu host rời phòng khi đang PENDING → có thể chuyển host cho người khác hoặc xoá phòng.
        // Bước 1: đơn giản là xoá người chơi ra khỏi phòng.

        nguoiChoiTranDauRepository.delete(nctd);

        int soNguoi = (int) nguoiChoiTranDauRepository.countByTranDau_Id(tranDau.getId());
        wsPublisher.publishPlayerLeft(tranDau.getId(), nctd.getNguoiDung().getId(), nctd.getNguoiDung().getHoTen(), soNguoi);

//        // Nếu không còn ai trong phòng → xoá phòng
//        long remain = nguoiChoiTranDauRepository.countByTranDau_Id(tranDau.getId());

        if (Objects.equals(tranDau.getChuPhong().getId(), currentUserId) && soNguoi > 0) {
            nguoiChoiTranDauRepository.findFirstByTranDau_IdOrderByIdAsc(tranDau.getId())
                    .ifPresent(next -> tranDau.setChuPhong(next.getNguoiDung()));
        }

        // Nếu phòng trống và chưa bắt đầu → xoá

        if (soNguoi == 0 && TrangThaiTranDau.PENDING.equals(tranDau.getTrangThai()))
            tranDauRepository.delete(tranDau);

        updateAndBroadcastLeaderboard(tranDau.getId(), null);

    }

    @Transactional(readOnly = true)
    @Override
    public TranDau chiTietPhong(Long tranDauId) throws Exception {
        return tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));
    }

    @Transactional(readOnly = true)
    @Override
    public TranDauResponse getBattleDetailResponse(Long tranDauId) throws Exception {
        // 1. Lấy thông tin trận đấu
        TranDau td = tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        // 2. 🔥 Tính số lượng người chơi hiện tại trong phòng
        int soLuong = (int) nguoiChoiTranDauRepository.countByTranDau_Id(tranDauId);

        // 3. Map sang DTO và trả về (truyền số lượng vào)
        return TranDauResponse.fromEntity(td, soLuong);
    }

    @Transactional(readOnly = true)
    @Override
    public TranDauResponse getBattleDetailResponse(Long tranDauId, Long currentUserId) throws Exception {
        TranDau td = tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        int soLuong = (int) nguoiChoiTranDauRepository.countByTranDau_Id(tranDauId);

        // 👇 kiểm tra user hiện tại có đang ở bảng nguoi_choi_tran_dau không
        boolean daThamGia = nguoiChoiTranDauRepository
                .existsByTranDauIdAndNguoiDungId(tranDauId, currentUserId);

        TranDauResponse res = TranDauResponse.fromEntity(td, soLuong);
        res.setDaThamGia(daThamGia);
        return res;
    }


    @Transactional(readOnly = true)
    @Override
    public Page<TranDau> danhSachPhongCho(PageRequest pageRequest) {
        return tranDauRepository.findByTrangThai(TrangThaiTranDau.PENDING, pageRequest);
    }

    @Override
    @Transactional
    public BattleStartResponse startBattle(Long tranDauId, Long currentUserId) throws Exception {
        TranDau td = tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        // 🔒 Chỉ chủ phòng mới được bắt đầu
        if (!td.getChuPhong().getId().equals(currentUserId)) {
            throw new SecurityException("Chỉ chủ phòng mới có quyền bắt đầu trận");
        }

        // ⛔ Không được start nếu đang không ở trạng thái chờ
        if (!TrangThaiTranDau.PENDING.equals(td.getTrangThai())) {
            throw new IllegalStateException("Phòng không ở trạng thái chờ");
        }

        // 📋 Lấy danh sách câu hỏi
        List<CauHoi> danhSachCauHoi = cauHoiRepository.findByBoCauHoiId(td.getBoCauHoi().getId());
        if (danhSachCauHoi.isEmpty()) {
            throw new IllegalStateException("Bộ câu hỏi này không có câu hỏi nào");
        }

        // 🔀 Trộn câu hỏi
        Collections.shuffle(danhSachCauHoi);

        // 🧭 Cập nhật DB
        td.setTrangThai(TrangThaiTranDau.ONGOING);
        td.setBatDauLuc(Instant.now());
        tranDauRepository.save(td);

        // 🧠 Khởi tạo BattleState mới trong RAM
        BattleState state = new BattleState();
        state.setTranDauId(td.getId());
        state.setDanhSachCauHoi(danhSachCauHoi);
        state.setStartTime(Instant.now());

        // ⏱ Thiết lập thời gian mỗi câu
        int seconds = (td.getGioiHanThoiGianCauGiay() != null)
                ? td.getGioiHanThoiGianCauGiay()
                : 15;
        state.setSecondsPerQuestion(seconds);

        // 🧍‍♂️ Khởi tạo điểm 0 cho toàn bộ người chơi
        Set<Long> playerIds = nguoiChoiTranDauRepository
                .findByTranDau_Id(td.getId(), null)
                .stream()
                .map(nctd -> nctd.getNguoiDung().getId())
                .collect(Collectors.toSet());
        // cũng thêm cả host vào
        playerIds.add(td.getChuPhong().getId());
        state.initScoresForPlayers(playerIds);

        // Lưu state vào manager
        battleStateManager.save(state);

        wsPublisher.publishBattleStarted(td.getId(), td.getTenPhong(), td.getBatDauLuc(), danhSachCauHoi.size(), seconds, 10);
        battleLoopTask.runAutoLoop(td.getId(), seconds);

        // ✅ Trả kết quả khởi tạo
        return BattleStartResponse.from(td, danhSachCauHoi);
    }

    @Override
    @Transactional
    public SubmitAnswerResponse submitAnswer(SubmitAnswerDTO dto, Long currentUserId) throws Exception {
        // 1️⃣ Kiểm tra trận đấu hợp lệ
        TranDau td = tranDauRepository.findById(dto.getTranDauId())
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        if (!TrangThaiTranDau.ONGOING.equals(td.getTrangThai())) {
            throw new IllegalStateException("Phòng không ở trạng thái đang diễn ra");
        }

        // 2️⃣ Xác nhận người chơi có trong phòng
        boolean inRoom = nguoiChoiTranDauRepository
                .findByTranDau_IdAndNguoiDung_Id(td.getId(), currentUserId)
                .isPresent();
        if (!inRoom && !Objects.equals(td.getChuPhong().getId(), currentUserId)) {
            throw new SecurityException("Bạn không ở trong phòng này");
        }

        // 3️⃣ Lấy BattleState hiện tại
        BattleState state = battleStateManager.get(td.getId());
        if (state == null || state.getCurrentQuestionIndex() < 0) {
            throw new IllegalStateException("Chưa có câu hỏi nào đang bật");
        }

        int idx = state.getCurrentQuestionIndex();
        CauHoi q = state.getDanhSachCauHoi().get(idx);

        if (!Objects.equals(q.getId(), dto.getCauHoiId())) {
            throw new IllegalArgumentException("Câu hỏi không khớp với câu hiện tại");
        }

        // 4️⃣ Ghi nhận đáp án (atomic)
        String ans = dto.getAnswer().trim().toUpperCase();
        boolean firstSubmit = state.recordAnswer(idx, currentUserId, ans);
        if (!firstSubmit) {
            throw new IllegalStateException("Bạn đã nộp đáp án cho câu này rồi");
        }

        // 5️⃣ Kiểm tra timeout
        int seconds = state.getSecondsPerQuestion();
        long totalMs = seconds * 1000L;
        long elapsedMs = Duration.between(state.getCurrentQuestionStart(), Instant.now()).toMillis();
        boolean withinTime = elapsedMs <= totalMs;

        // 6️⃣ Tính điểm
        boolean correct = withinTime && ans.equalsIgnoreCase(String.valueOf(q.getDapAnDung()));
        int gained = 0;
        if (correct) {
            if (LuatTinhDiem.SPEED_BONUS.equalsIgnoreCase(td.getLuatTinhDiem())) {
                long remain = Math.max(0, totalMs - elapsedMs);
                double ratio = (double) remain / (double) totalMs;
                gained = (int) Math.max(100, Math.round(1000 * ratio));
            } else {
                gained = 100;
            }
        }

        // 7️⃣ Cập nhật điểm tổng
        int total = state.addScore(currentUserId, gained);
        battleStateManager.save(state);

        // ⭐ 7.1) Cập nhật DB: diem & so_cau_dung của người nộp
        NguoiDung user = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        NguoiChoiTranDau player = nguoiChoiTranDauRepository
                .findByTranDau_IdAndNguoiDung_Id(td.getId(), user.getId())
                .orElseThrow(() -> new DataNotFoundException("Bạn chưa tham gia trận"));

        int cur = Optional.ofNullable(player.getDiem()).orElse(0);
        player.setDiem(cur + gained);
        if (correct) {
            player.setSoCauDung(Optional.ofNullable(player.getSoCauDung()).orElse(0) + 1);
        }
        nguoiChoiTranDauRepository.save(player);
        traLoiTranDauRepository.save(TraLoiTranDau.builder()
                .tranDau(td)
                .nguoiDung(user)
                .cauHoi(q)
                .luaChon(ans.charAt(0))
                .dungHaySai(correct)
                .thoiGianMs((int) elapsedMs)
                .build());

        // 9️⃣ Phát sự kiện WS cập nhật điểm cho người chơi này
        wsPublisher.publishScoreUpdate(
                td.getId(), user.getId(), user.getHoTen(), correct, gained, total, idx
        );

        // 🔟 Cập nhật và broadcast leaderboard tổng thể
//        updateAndBroadcastLeaderboard(td.getId(), state);
        updateAndBroadcastLeaderboard(td.getId());
        // 1️⃣1️⃣ Trả response cho client
        return SubmitAnswerResponse.builder()
                .correct(correct)
                .gainedPoints(gained)
                .totalPoints(total)
                .questionIndex(idx)
                .build();
    }

    @Override
    @Transactional
    public BattleFinishResponse finishBattle(Long tranDauId, Long currentUserId, boolean autoMode) throws Exception {
        System.out.println(">>> [SERVICE] finishBattle CALLED, tranDauId=" + tranDauId
                + ", currentUserId=" + currentUserId + ", autoMode=" + autoMode);

        TranDau td = tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        System.out.println(">>> [SERVICE] tran_dau.trang_thai = " + td.getTrangThai());

        // 1️⃣ Quyền hạn
        if (!autoMode && !td.getChuPhong().getId().equals(currentUserId)) {
            System.out.println("❌ [SERVICE] finishBattle: currentUserId KHÔNG phải chủ phòng");
            throw new SecurityException("Chỉ chủ phòng mới có thể kết thúc trận đấu");
        }

        // Đã kết thúc rồi → trả kết quả cũ, KHÔNG publish WS nữa
        if (TrangThaiTranDau.FINISHED.equals(td.getTrangThai())) {
            System.out.println("⚠️ [SERVICE] Trận đấu đã ở trạng thái FINISHED, trả BattleFinishResponse cũ");
            return BattleFinishResponse.from(td, null, null);
        }

        // 2️⃣ Lấy state trong RAM (nếu còn)
        BattleState state = battleStateManager.get(tranDauId);
        if (state != null && !state.markFinishedOnce()) {
            // Có người khác finish trước rồi
            System.out.println("⚠️ [SERVICE] markFinishedOnce = false, có luồng khác đã finish trước");
            return BattleFinishResponse.from(td, state.getDiemNguoiChoi(), null);
        }

        // 3️⃣ Lấy danh sách người chơi trong phòng
        List<NguoiChoiTranDau> players = nguoiChoiTranDauRepository.findAllByTranDau_Id(td.getId());
        System.out.println(">>> [SERVICE] So nguoi_choi_tran_dau = " + players.size());

        if (players.isEmpty()) {
            // Không có người chơi → chỉ đánh dấu FINISHED
            System.out.println("⚠️ [SERVICE] Không có người chơi nào, chỉ set FINISHED và return");
            td.setTrangThai(TrangThaiTranDau.FINISHED);
            td.setKetThucLuc(Instant.now());
            tranDauRepository.save(td);
            battleStateManager.remove(tranDauId);
            return BattleFinishResponse.from(td, null, null);
        }

        // 4️⃣ Map điểm (ưu tiên BattleState)
        Map<Long, Integer> scoreMap = new HashMap<>();
        if (state != null && state.getDiemNguoiChoi() != null && !state.getDiemNguoiChoi().isEmpty()) {
            scoreMap.putAll(state.getDiemNguoiChoi());
            System.out.println(">>> [SERVICE] scoreMap lấy từ BattleState size = " + scoreMap.size());
        } else {
            // fallback: từ bảng nguoi_choi_tran_dau
            for (NguoiChoiTranDau p : players) {
                scoreMap.put(
                        p.getNguoiDung().getId(),
                        p.getDiem() != null ? p.getDiem() : 0
                );
            }
            System.out.println(">>> [SERVICE] scoreMap fallback từ nguoi_choi_tran_dau size = " + scoreMap.size());
        }

        // 5️⃣ Lấy log trả lời để tính số câu đúng + tổng thời gian
        List<TraLoiTranDau> logs = traLoiTranDauRepository.findAllByTranDau_Id(td.getId());
        System.out.println(">>> [SERVICE] So tra_loi_tran_dau = " + logs.size());

        Map<Long, Integer> correctMap = new HashMap<>();
        Map<Long, Integer> totalTimeMap = new HashMap<>();

        for (TraLoiTranDau log : logs) {
            Long uid = log.getNguoiDung().getId();

            if (Boolean.TRUE.equals(log.getDungHaySai())) {
                correctMap.merge(uid, 1, Integer::sum);
            }
            if (log.getThoiGianMs() != null) {
                totalTimeMap.merge(uid, log.getThoiGianMs(), Integer::sum);
            }
        }

        // 6️⃣ Cập nhật điểm & số câu đúng vào nguoi_choi_tran_dau
        for (NguoiChoiTranDau p : players) {
            Long uid = p.getNguoiDung().getId();
            int diem = scoreMap.getOrDefault(uid, 0);
            int soCauDung = correctMap.getOrDefault(uid, 0);

            p.setDiem(diem);
            p.setSoCauDung(soCauDung);
        }

        // Xếp hạng theo điểm giảm dần
        players.sort(Comparator.comparing(NguoiChoiTranDau::getDiem).reversed());
        AtomicInteger rankCounter = new AtomicInteger(1);
        players.forEach(p -> p.setXepHang(rankCounter.getAndIncrement()));
        nguoiChoiTranDauRepository.saveAll(players);
        System.out.println(">>> [SERVICE] Đã cập nhật diem/so_cau_dung/xep_hang cho nguoi_choi_tran_dau");

        // 7️⃣ Cập nhật winner + trạng thái trận
        NguoiChoiTranDau winnerPlayer = players.get(0);
        td.setWinner(winnerPlayer.getNguoiDung());        // => sẽ update winner_id
        td.setTrangThai(TrangThaiTranDau.FINISHED);
        td.setKetThucLuc(Instant.now());
        tranDauRepository.save(td);
        System.out.println(">>> [SERVICE] Winner = " + winnerPlayer.getNguoiDung().getHoTen()
                + ", diem = " + winnerPlayer.getDiem());

        // 8️⃣ Lưu lịch sử trận đấu
        Instant now = Instant.now();
        List<LichSuTranDau> lichSuList = players.stream()
                .map(p -> {
                    Long uid = p.getNguoiDung().getId();
                    Integer tongTime = totalTimeMap.getOrDefault(uid, 0);
                    return LichSuTranDau.builder()
                            .tranDau(td)
                            .nguoiDung(p.getNguoiDung())
                            .tongDiem(p.getDiem())
                            .soCauDung(p.getSoCauDung())
                            .tongThoiGianMs(tongTime)
                            .xepHang(p.getXepHang())
                            .hoanThanhLuc(now)
                            .build();
                })
                .toList();

        lichSuTranDauRepository.saveAll(lichSuList);
        // Tìm điểm cao nhất
        int maxScore = players.get(0).getDiem();

        // Tập user thắng (có thể >1 nếu hòa điểm)
        Set<Long> winnerIds = players.stream()
                .filter(p -> p.getDiem() == maxScore)
                .map(p -> p.getNguoiDung().getId())
                .collect(Collectors.toSet());

        // cập nhật BXH theo best-score + winners
        updateRankingAfterBattle(td, scoreMap, winnerIds);

        System.out.println(">>> [SERVICE] Đã lưu lich_su_tran_dau, size=" + lichSuList.size());

        // 9️⃣ Phát WS FINISHED event
        FinishedEvent.Winner winData = FinishedEvent.Winner.builder()
                .userId(winnerPlayer.getNguoiDung().getId())
                .hoTen(winnerPlayer.getNguoiDung().getHoTen())
                .diem(winnerPlayer.getDiem())
                .soCauDung(winnerPlayer.getSoCauDung())
                .build();

        System.out.println("🔥 [SERVICE] Chuẩn bị publish FINISHED WS cho tran_dau_id = " + td.getId()
                + ", so_nguoi_choi = " + players.size());

        wsPublisher.publishFinished(
                td.getId(),
                td.getTenPhong(),
                td.getMaPhong(),
                td.getBatDauLuc(),
                td.getKetThucLuc(),
                winData,
                players.stream()
                        .map(p -> FinishedEvent.Player.builder()
                                .userId(p.getNguoiDung().getId())
                                .hoTen(p.getNguoiDung().getHoTen())
                                .diem(p.getDiem())
                                .soCauDung(p.getSoCauDung())
                                .xepHang(p.getXepHang())
                                .build())
                        .toList()
        );

        // 🔟 Dọn state trong RAM
        battleStateManager.remove(tranDauId);
        System.out.println(">>> [SERVICE] Đã remove BattleState khỏi RAM");

        // 🔁 Build response REST
        Map<Long, Integer> finalScores = players.stream()
                .collect(Collectors.toMap(
                        p -> p.getNguoiDung().getId(),
                        NguoiChoiTranDau::getDiem
                ));

        List<NguoiDung> allUsers = players.stream()
                .map(NguoiChoiTranDau::getNguoiDung)
                .toList();

        return BattleFinishResponse.from(td, finalScores, allUsers);
    }


    @Transactional(readOnly = true)
    @Override
    public SyncStateResponse syncState(Long tranDauId, Long currentUserId) throws Exception {
        TranDau td = tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        BattleState state = battleStateManager.get(tranDauId);
        int seconds = td.getGioiHanThoiGianCauGiay() != null
                ? td.getGioiHanThoiGianCauGiay()
                : 15;
        if (state == null || !TrangThaiTranDau.ONGOING.equals(td.getTrangThai())) {
            return SyncStateResponse.builder()
                    .tranDauId(td.getId())
                    .trangThai(td.getTrangThai())
                    .secondsPerQuestion(seconds)
                    .currentQuestionIndex(-1)
                    .myTotalPoints(0)
                    .build();
        }

        int idx = state.getCurrentQuestionIndex();
        CauHoi q = (idx >= 0 && idx < state.getDanhSachCauHoi().size())
                ? state.getDanhSachCauHoi().get(idx)
                : null;

        return SyncStateResponse.builder()
                .tranDauId(td.getId())
                .trangThai(td.getTrangThai())
                .currentQuestionIndex(idx)
                .currentQuestionStart(state.getCurrentQuestionStart())
                .secondsPerQuestion(seconds)
                .currentQuestionId(q != null ? q.getId() : null)
                .noiDung(q != null ? q.getNoiDung() : null)
                .loaiNoiDung(q != null ? q.getLoaiNoiDung() : null)
                .duongDanTep(q != null ? q.getDuongDanTep() : null)
                .a(q != null ? q.getLuaChonA() : null)
                .b(q != null ? q.getLuaChonB() : null)
                .c(q != null ? q.getLuaChonC() : null)
                .d(q != null ? q.getLuaChonD() : null)
                .myTotalPoints(currentUserId != null
                        ? state.getDiemNguoiChoi().getOrDefault(currentUserId, 0)
                        : 0)
                .build();
    }

    private void updateRankingAfterBattle(TranDau td,
                                          Map<Long, Integer> scores,
                                          Set<Long> winnerIds) {
        for (var e : scores.entrySet()) {
            Long userId = e.getKey();

            // Không cho điểm âm ảnh hưởng BXH
            int rawScore = e.getValue() != null ? e.getValue() : 0;
            int diemTranNay = Math.max(0, rawScore);

            Long boCauHoiId = td.getBoCauHoi().getId();

            // 1. Lấy record best-score hiện tại (nếu có)
            ThanhTichBoCauHoi thanhTich = thanhTichBoCauHoiRepository
                    .findByNguoiDung_IdAndBoCauHoi_Id(userId, boCauHoiId)
                    .orElse(null);

            int oldBest = (thanhTich != null) ? thanhTich.getDiemCaoNhat() : 0;
            int delta = 0;

            if (thanhTich == null) {
                // Chưa từng chơi bộ này => best-score mới
                delta = diemTranNay;

                thanhTich = ThanhTichBoCauHoi.builder()
                        .nguoiDung(nguoiDungRepository.getReferenceById(userId))
                        .boCauHoi(td.getBoCauHoi())
                        .diemCaoNhat(diemTranNay)
                        .tranDau(td) // trận đầu tiên cũng là best
                        .build();
            } else if (diemTranNay > oldBest) {
                // Cải thiện kỷ lục
                delta = diemTranNay - oldBest;
                thanhTich.setDiemCaoNhat(diemTranNay);
                thanhTich.setTranDau(td);
            } else {
                // Không cải thiện => không cộng điểm rank
                delta = 0;
            }

            thanhTichBoCauHoiRepository.save(thanhTich);

            // 2. Cập nhật bảng xếp hạng tổng
            BangXepHang bxh = bangXepHangRepository.findByNguoiDung_Id(userId)
                    .orElse(BangXepHang.builder()
                            .nguoiDung(nguoiDungRepository.getReferenceById(userId))
                            .tongDiem(0)
                            .tongTran(0)
                            .soTranThang(0)
                            .soTranThua(0)
                            .build());

            // Mỗi lần kết thúc trận -> +1 tổng trận
            bxh.setTongTran(bxh.getTongTran() + 1);

            // cộng delta (nếu > 0) vào tổng điểm
            if (delta > 0) {
                bxh.setTongDiem(bxh.getTongDiem() + delta);
            }

            // --- Thắng / thua / AFK ---
            // Người thắng: thuộc winnerIds
            boolean isWinner = winnerIds != null && winnerIds.contains(userId);

            // AFK/0 điểm: không tính là thua để thống kê đẹp hơn
            if (isWinner) {
                bxh.setSoTranThang(bxh.getSoTranThang() + 1);
            } else if (diemTranNay > 0) {
                // chỉ những người có >0 điểm mới tính là thua
                bxh.setSoTranThua(bxh.getSoTranThua() + 1);
            }
            // còn lại (0 điểm, không thuộc winner) -> coi như tham gia nhưng ko +thắng cũng ko +thua
            bangXepHangRepository.save(bxh);

        }
    }

    @Override
    public Page<LichSuTranDauResponse> getMyHistory(Long currentUserId, int page, int limit) {
        PageRequest pageable = PageRequest.of(page, limit);
        return lichSuTranDauRepository
                .findByNguoiDung_IdOrderByHoanThanhLucDesc(currentUserId, pageable)
                .map(LichSuTranDauResponse::fromEntity);
    }

    @Override
    public Page<LichSuTranDauResponse> getUserHistory(Long userId, int page, int limit) {
        PageRequest pageable = PageRequest.of(page, limit);
        return lichSuTranDauRepository
                .findByNguoiDung_IdOrderByHoanThanhLucDesc(userId, pageable)
                .map(LichSuTranDauResponse::fromEntity);
    }


    @Override
    public LichSuTranDauDetailResponse getMyHistoryDetail(Long tranDauId, Long currentUserId) throws Exception {
        TranDau td = tranDauRepository.findById(tranDauId)
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        LichSuTranDau myHistory = lichSuTranDauRepository
                .findByTranDau_IdAndNguoiDung_Id(tranDauId, currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Bạn chưa tham gia trận đấu này"));

        // base info
        LichSuTranDauDetailResponse res = LichSuTranDauDetailResponse.baseFrom(td, myHistory);

        // leaderboard
        List<LichSuTranDau> all = lichSuTranDauRepository
                .findByTranDau_IdOrderByXepHangAsc(tranDauId);

        List<FinishedPlayer> leaderboard = all.stream()
                .map(ls -> FinishedPlayer.builder()
                        .userId(ls.getNguoiDung().getId())
                        .hoTen(ls.getNguoiDung().getHoTen())
                        .diem(ls.getTongDiem())
                        .soCauDung(ls.getSoCauDung())
                        .xepHang(ls.getXepHang())
                        .build())
                .toList();

        res.setLeaderboard(leaderboard);

        // câu hỏi / đáp án của riêng user
        List<TraLoiTranDau> answers = traLoiTranDauRepository
                .findByTranDau_IdAndNguoiDung_IdOrderByTraLoiLucAsc(tranDauId, currentUserId);

        List<LichSuTranDauQuestionResponse> qList = answers.stream()
                .map(tl -> LichSuTranDauQuestionResponse.fromEntities(
                        tl,
                        tl.getCauHoi()      // đã quan hệ @ManyToOne
                ))
                .toList();
        res.setQuestions(qList);
        return res;
    }

    @Override
    public void guiChatTrongTran(GuiChatDTO dto, Long currentUserId) throws Exception {
        NguoiDung nguoiDung = nguoiDungRepository.findById(currentUserId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));

        TranDau tranDau = tranDauRepository.findById(dto.getTranDauId())
                .orElseThrow(() -> new DataNotFoundException("Trận đấu không tồn tại"));

        // Không cho chat ở trận đã kết thúc (tuỳ bạn)
        if (tranDau.getTrangThai() == TrangThaiTranDau.FINISHED) {
            throw new IllegalStateException("Trận đấu đã kết thúc, không thể chat");
        }

        // Bắt buộc phải là người trong phòng
        boolean joined = nguoiChoiTranDauRepository
                .existsByTranDauIdAndNguoiDungId(tranDau.getId(), nguoiDung.getId());
        if (!joined) {
            throw new PermissionDenyException("Bạn chưa tham gia trận đấu này");
        }

        // Không lưu DB, chỉ broadcast WS
        wsPublisher.publishChatMessage(
                tranDau.getId(),
                nguoiDung.getId(),
                nguoiDung.getHoTen(),
                dto.getNoiDung(),
                false // system = false
        );
    }

    private void updateAndBroadcastLeaderboard(Long tranDauId, BattleState optionalState) {
        BattleState state = (optionalState != null)
                ? optionalState : battleStateManager.get(tranDauId);
        if (state == null) return;

        Map<Long, Integer> scores = state.getDiemNguoiChoi();
        if (scores.isEmpty()) return;

        List<Long> ids = new ArrayList<>(scores.keySet());
        Map<Long, String> nameMap = nguoiDungRepository.findAllById(ids).stream()
                .collect(Collectors.toMap(NguoiDung::getId, NguoiDung::getHoTen));

        AtomicInteger rank = new AtomicInteger(1);
        List<LeaderboardUpdateEvent.Row> board = scores.entrySet().stream()
                .sorted(Map.Entry.<Long, Integer>comparingByValue().reversed())
                .map(e -> LeaderboardUpdateEvent.Row.builder()
                        .userId(e.getKey())
                        .hoTen(nameMap.getOrDefault(e.getKey(), "Người chơi"))
                        .diem(e.getValue())
                        .xepHang(rank.getAndIncrement()).build())
                .toList();

        wsPublisher.publishLeaderboard(tranDauId, board);
    }


    @Transactional
    public void updateAndBroadcastLeaderboard(Long tranDauId) {
        List<NguoiChoiTranDau> all = nguoiChoiTranDauRepository.findAllByTranDau_Id(tranDauId);

        // Sắp xếp: điểm giảm dần, cùng điểm thì ai vào trước xếp cao hơn
        all.sort(Comparator
                .comparing(NguoiChoiTranDau::getDiem, Comparator.nullsFirst(Comparator.naturalOrder())).reversed()
                .thenComparing(NguoiChoiTranDau::getThamGiaLuc, Comparator.nullsFirst(Comparator.naturalOrder())));

        int rank = 1;
        for (NguoiChoiTranDau p : all) p.setXepHang(rank++);
        nguoiChoiTranDauRepository.saveAll(all);

        var rows = all.stream().map(p -> LeaderboardUpdateEvent.Row.builder()
                .userId(p.getNguoiDung().getId())
                .hoTen(p.getNguoiDung().getHoTen())
                .diem(Optional.ofNullable(p.getDiem()).orElse(0))
                .soCauDung(Optional.ofNullable(p.getSoCauDung()).orElse(0))
                .xepHang(Optional.ofNullable(p.getXepHang()).orElse(0))
                .build()).toList();

        wsPublisher.publishLeaderboard(tranDauId, rows);
    }

}
